use std::cell::UnsafeCell;

use util::iter::{KnownLen, OptionIter};
use util::list::List;
use util::loc::Loc;
use util::sym::Sym;
use util::up::Up;
use util::utils::todo;

use ast;

use model::class::{ClassDeclaration, ClassHead, MemberDeclaration, SlotDeclaration};
use model::diag::Diag;
use model::effect::Effect;
use model::expr::{Case, Catch, Expr, ExprData, Local, Pattern};
use model::method::{InstMethod, MethodOrImplOrAbstract, MethodOrImpl, Parameter};
use model::ty::{InstClass, Ty};

use super::class_utils::{try_get_member_of_inst_class, InstMember};
use super::ctx::Ctx;
use super::expected::Expected;
use super::instantiator::Instantiator;
use super::ty_utils::{common_ty, instantiate_and_narrow_effects, instantiate_ty, is_assignable};

pub fn check_method_body<
	'ast,
	'ctx,
	'instantiator,
	'builtins_ctx: 'ctx,
	'model: 'ctx + 'instantiator + 'builtins_ctx,
>(
	ctx: &'ctx mut Ctx<'builtins_ctx, 'model>,
	method_or_impl: MethodOrImpl<'model>,
	method_instantiator: &'instantiator Instantiator<'model>,
	is_static: bool,
	body: &'ast ast::Expr<'ast>,
) -> &'model Expr<'model> {
	let signature = method_or_impl.signature();
	let mut ectx = CheckExprContext::<'ctx, 'instantiator, 'builtins_ctx, 'model> {
		ctx,
		method_or_impl,
		method_instantiator,
		is_static,
		self_effect: signature.self_effect,
		current_parameters: signature.parameters,
		locals: Vec::new(),
	};
	let return_ty = ectx.instantiate_ty(&signature.return_ty, method_instantiator);
	ectx.check_return(&return_ty, body)
}

struct CheckExprContext<
	'ctx,
	'instantiator,
	'builtins_ctx: 'ctx,
	'model: 'ctx + 'instantiator + 'builtins_ctx,
> {
	ctx: &'ctx mut Ctx<'builtins_ctx, 'model>,
	method_or_impl: MethodOrImpl<'model>,
	method_instantiator: &'instantiator Instantiator<'model>,
	is_static: bool,
	self_effect: Effect,
	current_parameters: &'model [Parameter<'model>],
	locals: Vec<&'model Local<'model>>,
}
impl<'ctx, 'instantiator, 'builtins_ctx, 'model>
	CheckExprContext<'ctx, 'instantiator, 'builtins_ctx, 'model> {
	fn add_diagnostic(&mut self, loc: Loc, data: Diag<'model>) {
		self.ctx.add_diagnostic(loc, data)
	}

	//TODO:PERF use placement new to avoid copying stuff into here!
	fn alloc(&mut self, e: Expr<'model>) -> &'model Expr<'model> {
		self.ctx.arena <- e
	}

	fn check_return<'ast>(&mut self, ty: &Ty<'model>, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(Expected::Return(ty), a)
	}

	//TODO: this is always called immediately after instantiate_ty, combine?
	fn check_subtype<'ast>(&mut self, ty: &Ty<'model>, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(Expected::SubTypeOf(ty), a)
	}

	fn check_void<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		let void = self.ctx.expected_void();
		self.check_expr(void, a)
	}

	fn check_bool<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		let bool = self.ctx.expected_bool();
		self.check_expr(bool, a)
	}

	fn check_infer<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		let inferred = UnsafeCell::new(None);
		self.check_expr(Expected::Infer(&inferred), a)
	}

	fn check_expr<'ast, 'expected>(
		&mut self,
		e: Expected<'expected, 'model>,
		a: &'ast ast::Expr<'ast>,
	) -> &'model Expr<'model> {
		self.check_expr_worker(e, a).0
	}

	fn check_expr_worker<'ast, 'expected>(
		&mut self,
		e: Expected<'expected, 'model>,
		&ast::Expr { loc, data: ref ast_data }: &'ast ast::Expr<'ast>,
	) -> Handled<'model> {
		match *ast_data {
			ast::ExprData::Access(name) => {
				if let Some(local) = self.try_find_local(name) {
					return self.handle(e, loc, local.ty.clone(), ExprData::AccessLocal(Up(local)))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = self.instantiate_ty(&param.ty, self.method_instantiator);
					return self.handle(e, loc, ty, ExprData::AccessParameter(Up(param)))
				}

				if let Some(InstMember(decl, instantiator)) =
					self.ctx.get_own_member_or_add_diagnostic(loc, name)
				{
					return match decl {
						MemberDeclaration::Slot(slot) => self.get_own_slot(e, loc, slot, instantiator),
						MemberDeclaration::Method(_) | MemberDeclaration::AbstractMethod(_) =>
							unimplemented!(), //diagnostic
					}
				}

				self.bogus(loc)
			}
			ast::ExprData::StaticAccess { .. } | ast::ExprData::TypeArguments(_) => {
				self.add_diagnostic(loc, Diag::MethodUsedAsValue);
				self.bogus(loc)
			}
			ast::ExprData::OperatorCall(&ast::OperatorCallData { ref left, operator, ref right }) => {
				let ty_args = todo(); //List::empty(); // No way to provide these to an operator call.
				self.call_method(e, loc, left, operator, ty_args, OptionIter(Some(right)))
			}
			ast::ExprData::Call(&ast::CallData { ref target, args }) =>
				self.check_call_ast_worker(e, loc, target, args),
			ast::ExprData::Recur(arg_asts) => {
				if !e.in_tail_call_position() {
					self.add_diagnostic(loc, Diag::NotATailCall)
				}
				// For recursion, need to do substitution in case we are
				// implementing an abstract method where the superclass took type arguments.
				let method_or_impl = self.method_or_impl;
				let moa = method_or_impl.method_or_abstract();
				let inst = self.method_instantiator;
				let args = unwrap_or_return!(self.check_arguments(loc, moa, inst, arg_asts), self.bogus(loc));
				self.handle(e, loc, method_or_impl.return_ty().clone(), ExprData::Recur(method_or_impl, args))
			}
			ast::ExprData::New(&ast::NewData(ty_arg_asts, arg_asts)) => {
				let slots = match *self.ctx.current_class.head {
					ClassHead::Slots(_, slots) => slots,
					_ => {
						let diag = Diag::NewInvalid(self.ctx.current_class);
						self.add_diagnostic(loc, diag);
						return self.bogus(loc)
					}
				};
				if arg_asts.len() != slots.len() {
					let diag = Diag::NewArgumentCountMismatch {
						class: self.ctx.current_class,
						n_slots: slots.len(),
						n_arguments: arg_asts.len(),
					};
					self.add_diagnostic(
						loc,
						diag,
					);
					return self.bogus(loc)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len() {
					unimplemented!()
				}

				let inst_class = unwrap_or_return!(
					{ let current_class = self.ctx.current_class; self.ctx.instantiate_class(current_class, ty_arg_asts) },
					self.bogus(loc)
				);
				let instantiator = Instantiator::of_inst_class(&inst_class);
				let args = self.ctx.arena.map(slots.zip(arg_asts), |(slot, arg)| {
					let ty = self.instantiate_ty(&slot.ty, &instantiator);
					self.check_subtype(&ty, arg)
				});
				let ty = Ty::Plain(Effect::MAX, inst_class);
				self.handle(e, loc, ty, ExprData::New(args))
			}
			ast::ExprData::ArrayLiteral(&ast::ArrayLiteralData(ref element_ty, args)) => {
				unused!(element_ty, args);
				unimplemented!()
			}
			ast::ExprData::GetProperty(&ast::GetPropertyData(ref target_ast, property_name)) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match target.ty {
					Ty::Bogus => return self.bogus(loc),
					Ty::Plain(target_effect, ref target_class) => {
						let InstMember(member_decl, instantiator) =
							//TODO: just get_slot_of_inst_class
							unwrap_or_return!(
								self.get_member_of_inst_class(target.loc, target_class, property_name),
								self.bogus(loc));
						let slot = match member_decl {
							MemberDeclaration::Slot(s) => s,
							_ => {
								self.add_diagnostic(target.loc, Diag::MethodUsedAsValue);
								return self.bogus(loc)
							}
						};
						if slot.mutable && !target_effect.can_get() {
							self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(Up(slot)))
						}
						let slot_ty = instantiate_and_narrow_effects(
							target_effect,
							&slot.ty,
							&instantiator,
							loc,
							&mut self.ctx.diags,
							self.ctx.arena,
						);
						(slot, slot_ty)
					}
					Ty::Param(_) => unimplemented!(),
				};
				self.handle(e, loc, slot_ty, ExprData::GetSlot(target, Up(slot)))
			}
			ast::ExprData::SetProperty(&ast::SetPropertyData(property_name, ref value_ast)) => {
				let InstMember(member_decl, instantiator) = unwrap_or_return!(
					self.ctx
						.get_own_member_or_add_diagnostic(loc, property_name),
					self.bogus(loc)
				);
				let slot = match member_decl {
					MemberDeclaration::Slot(s) => s,
					_ => {
						self.add_diagnostic(loc, Diag::CantSetNonSlot(member_decl));
						return self.bogus(loc)
					}
				};
				if !slot.mutable {
					self.add_diagnostic(loc, Diag::SlotNotMutable(Up(slot)))
				}
				let allowed_effect = self.self_effect;
				if !allowed_effect.can_set() {
					self.add_diagnostic(loc, Diag::MissingEffectToSetSlot { allowed_effect, slot: Up(slot) })
				}

				let slot_ty = self.instantiate_ty(&slot.ty, &instantiator);
				let value = self.ctx.arena <- self.check_subtype(&slot_ty, value_ast);
				let void = self.ctx.void();
				self.handle(e, loc, void, ExprData::SetSlot(Up(slot), value))
			}
			ast::ExprData::Let(&ast::LetData(ref pattern_ast, ref value_ast, ref then_ast)) => {
				let value = self.check_infer(value_ast);
				let (pattern, n_added) = {
					//self.start_check_pattern(value.ty(), pattern_ast);
					let &ast::Pattern(pattern_loc, ref pattern_data) = pattern_ast;
					match *pattern_data {
						ast::PatternData::Ignore => (Pattern::Ignore, 0),
						ast::PatternData::Single(name) => {
							let local =
								self.ctx.arena <- Local { loc: pattern_loc, ty: value.ty.clone(), name };
							self.add_to_scope(local);
							(Pattern::Single(local), 1)
						}
						ast::PatternData::Destruct(_) => unimplemented!(),
					}
				};
				let then = self.check_expr(e, then_ast);
				for _ in 0..n_added {
					self.pop_from_scope()
				}
				// 'expected' was handled in 'then'
				Handled(self.expr(loc, then.ty.clone(), ExprData::Let(pattern, value, then)))
			}
			ast::ExprData::Seq(&ast::SeqData(ref first_ast, ref then_ast)) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(e, then_ast);
				// 'expected' was handled in 'then'
				Handled(self.expr(loc, then.ty.clone(), ExprData::Seq(first, then)))
			}
			ast::ExprData::LiteralNat(_) => unimplemented!(),
			ast::ExprData::LiteralInt(_) => unimplemented!(),
			ast::ExprData::LiteralFloat(_) => unimplemented!(),
			ast::ExprData::LiteralString(_) => unimplemented!(),
			ast::ExprData::SelfExpr => {
				/*
				Create an InstClass for the current class that just maps type parameters to theirselves.
				For example:
					class Foo[T]
						Foo[T] get-self()
							self || This is of type Foo[T] where T is the same as the type parameter on Foo.

					fun use-it(Foo[Int] foo)
                        || The return type has the same T,
						|| so it will be instantiated to return Foo[Int].
						foo.get-self()
				*/
				let ty = Ty::Plain(self.self_effect, self.current_inst_class());
				self.handle(e, loc, ty, ExprData::SelfExpr)
			}
			ast::ExprData::IfElse(&ast::IfElseData(ref test_ast, ref then_ast, ref else_ast)) => {
				let test = self.check_bool(test_ast);
				let then = self.check_expr(e, then_ast);
				let elze = self.check_expr(e, else_ast);
				let ty = e.inferred_ty().clone();
				// 'expected' was handled in both 'then' and 'else'.
				Handled(self.expr(loc, ty, ExprData::IfElse { test, then, elze }))
			}
			ast::ExprData::WhenTest(&ast::WhenTestData { cases: case_asts, elze: ref else_ast }) => {
				let cases = self.ctx.arena.map(
					case_asts,
					|&ast::Case(case_loc, ref test_ast, ref result_ast)| {
						let test = self.check_bool(test_ast);
						let result = self.check_expr(e, result_ast);
						Case(case_loc, test, result)
					},
				);
				let elze = self.check_expr(e, else_ast);
				let ty = e.inferred_ty().clone();
				// 'expected' handled in each case result and in 'elze'.
				Handled(self.expr(loc, ty, ExprData::WhenTest(cases, elze)))
			}
			ast::ExprData::Assert(asserted_ast) => {
				let asserted = self.check_bool(asserted_ast);
				let void = self.ctx.void();
				self.handle(e, loc, void, ExprData::Assert(asserted))
			}
			ast::ExprData::Try(
				&ast::TryData { try: ref do_ast, catch: ref op_catch_ast, finally: ref op_finally_ast },
			) => {
				let body = self.check_expr(e, do_ast);
				let catch = op_catch_ast.as_ref().map(|catch| {
					let &ast::Catch {
						loc: catch_loc,
						exception_ty: ref exception_ty_ast,
						exception_name_loc,
						exception_name,
						then: ref then_ast
					} = catch;
					let exception_ty = self.ctx.get_ty(exception_ty_ast);
					let caught = self.ctx.arena <- Local {
                        loc: exception_name_loc,
                        ty: exception_ty,
                        name: exception_name
                    };
					self.add_to_scope(caught);
					let then = self.check_expr(e, then_ast);
					self.pop_from_scope();
					Catch(catch_loc, caught, then)
				});
				let finally = op_finally_ast.as_ref().map(|f| self.check_void(f));
				let ty = e.inferred_ty().clone();
				// 'expected' handled when checking 'do' and 'catch'
				Handled(self.expr(loc, ty, ExprData::Try { body, catch, finally }))
			}
			ast::ExprData::For(&ast::ForData { local_name, looper: ref looper_ast, body: ref body_ast }) => {
				unused!(local_name, looper_ast, body_ast);
				unimplemented!()
			}
		}
	}

	//mv
	fn try_find_local(&self, name: Sym) -> Option<&'model Local<'model>> {
		for local in &self.locals {
			if local.name == name {
				return Some(local)
			}
		}
		None
	}

	//mv
	fn add_to_scope(&mut self, local: &'model Local<'model>) {
		// It's important that we push even in the presence of errors, because we will always pop.
		if let Some(param) = self.current_parameters
			.iter()
			.find(|p| p.name == local.name)
		{
			self.add_diagnostic(local.loc, Diag::CantReassignParameter(Up(param)))
		}
		if let Some(old_local) = self.try_find_local(local.name) {
			self.add_diagnostic(local.loc, Diag::CantReassignLocal(Up(old_local)))
		}
		self.locals.place_back() <- local;
	}
	fn pop_from_scope(&mut self) {
		let popped = self.locals.pop();
		assert!(popped.is_some())
	}

	fn check_call_ast_worker<'ast, 'expected>(
		&mut self,
		expected: Expected<'expected, 'model>,
		loc: Loc,
		target: &'ast ast::Expr<'ast>,
		args: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let &ast::Expr { loc: target_loc, data: ref target_data } = target;
		let (&ast::Expr { loc: real_target_loc, data: ref real_target_data }, ty_arg_asts) = match *target_data {
			ast::ExprData::TypeArguments(&ast::TypeArgumentsData { target: ref real_target, type_arguments: ty_arg_asts }) =>
				(real_target, ty_arg_asts),
			_ => (target, List::EMPTY),
		};
		match *real_target_data {
			ast::ExprData::StaticAccess { class_name, static_method_name } => {
				let class = unwrap_or_return!(
					self.ctx
						.access_class_declaration_or_add_diagnostic(real_target_loc, class_name),
					self.bogus(target_loc)
				);
				self.call_static_method(expected, loc, class, static_method_name, ty_arg_asts, args)
			}
			ast::ExprData::GetProperty(&ast::GetPropertyData(ref property_target, property_name)) =>
				self.call_method(expected, loc, property_target, property_name, ty_arg_asts, args),
			ast::ExprData::Access(name) => self.call_own_method(expected, loc, name, ty_arg_asts, args),
			_ => {
				self.add_diagnostic(loc, Diag::CallsNonMethod);
				self.bogus(target_loc)
			}
		}
	}

	fn call_static_method<'ast, 'expected>(
		&mut self,
		expected: Expected<'expected, 'model>,
		loc: Loc,
		class: Up<'model, ClassDeclaration<'model>>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let method_decl = unwrap_or_return!(class.find_static_method(method_name), {
			self.add_diagnostic(loc, Diag::StaticMethodNotFound(class, method_name));
			self.bogus(loc)
		});

		let inst_method = unwrap_or_return!(
			self.ctx
				.instantiate_method(MethodOrImplOrAbstract::Method(Up(method_decl)), ty_arg_asts),
			self.bogus(loc)
		);

		// No need to check selfEffect, because this is a static method.
		// Static methods can't look at their class' type arguments
		let args = unwrap_or_return!(
			self.check_call_arguments(loc, inst_method, &Instantiator::NIL, arg_asts),
			self.bogus(loc)
		);

		let ty = self.instantiate_return_ty(inst_method);
		self.handle(expected, loc, ty, ExprData::StaticMethodCall { method: inst_method, args })
	}

	//mv
	fn current_inst_class(&mut self) -> InstClass<'model> {
		InstClass::generic_self_reference(self.ctx.current_class, self.ctx.arena)
	}

	fn call_own_method<'ast, 'expected>(
		&mut self,
		expected: Expected<'expected, 'model>,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		// Note: InstClass is still relevent here:
		// Even if 'self' is not an inst, in a superclass we will fill in type parameters.
		let current_inst_class = self.current_inst_class();
		let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
			self.get_member_of_inst_class(loc, &current_inst_class, method_name),
			self.bogus(loc)
		);

		//TODO: helper fn for converting member -> method
		let method_decl = match member_decl {
			MemberDeclaration::Method(m) => MethodOrImplOrAbstract::Method(Up(m)),
			MemberDeclaration::AbstractMethod(a) => MethodOrImplOrAbstract::Abstract(Up(a)),
			_ => {
				self.add_diagnostic(loc, Diag::CallsNonMethod);
				return self.bogus(loc)
			}
		};

		let inst_method =
			unwrap_or_return!(self.ctx.instantiate_method(method_decl, ty_arg_asts), self.bogus(loc));

		let args = unwrap_or_return!(
			self.check_call_arguments(loc, inst_method, &member_instantiator, arg_asts),
			self.bogus(loc)
		);

		let ty = self.instantiate_return_ty(inst_method);

		let expr = if method_decl.is_static() {
			// Calling own static method is OK.
			ExprData::StaticMethodCall { method: inst_method, args }
		} else {
			if self.is_static {
				self.add_diagnostic(loc, Diag::CantCallInstanceMethodFromStaticMethod(method_decl));
				return self.bogus(loc)
			}

			let target_effect = self.self_effect;
			if !target_effect.contains(method_decl.self_effect()) {
				self.add_diagnostic(loc, Diag::IllegalSelfEffect { target_effect, method: method_decl })
			}

			ExprData::MyInstanceMethodCall { method: inst_method, args }
		};

		self.handle(expected, loc, ty, expr)
	}

	fn call_method<'ast, 'expected, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		expected: Expected<'expected, 'model>,
		loc: Loc,
		target_ast: &'ast ast::Expr<'ast>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: I,
	) -> Handled<'model> {
		let target = self.check_infer(target_ast);
		let (inst_method, args, ty) = match target.ty {
			Ty::Bogus =>
				// Already issued an error, don't need another.
				return self.bogus(loc),
			Ty::Plain(target_effect, ref target_inst_class) => {
				let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
					self.get_member_of_inst_class(loc, target_inst_class, method_name),
					self.bogus(loc));

				let method = match member_decl {
					MemberDeclaration::Method(m) => MethodOrImplOrAbstract::Method(Up(m)),
					MemberDeclaration::AbstractMethod(a) => MethodOrImplOrAbstract::Abstract(Up(a)),
					_ => {
						self.add_diagnostic(loc, Diag::CallsNonMethod);
						return self.bogus(loc)
					}
				};

				if let MethodOrImplOrAbstract::Method(m) = method {
					if m.is_static {
						self.add_diagnostic(loc, Diag::CantAccessStaticMethodThroughInstance(m.clone_as_up()));
						return self.bogus(loc)
					}
				}

				if !target_effect.contains(method.self_effect()) {
					self.add_diagnostic(loc, Diag::IllegalSelfEffect { target_effect, method })
				}

				// Note: member is instantiated based on the *class* type arguments,
                // but there may sill be *method* type arguments.
				let inst_method = unwrap_or_return!(self.ctx.instantiate_method(method, ty_arg_asts), self.bogus(loc));

				let args = unwrap_or_return!(
					self.check_call_arguments(loc, inst_method, &member_instantiator, arg_asts),
					self.bogus(loc));

				let ty = self.instantiate_return_ty_with_extra_instantiator(inst_method, &member_instantiator);
				(inst_method, args, ty)
			}
			Ty::Param(_) =>
				unimplemented!()
		};
		self.handle(expected, loc, ty, ExprData::InstanceMethodCall { target, method: inst_method, args })
	}

	fn check_call_arguments<'ast, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		inst_method: &'model InstMethod<'model>,
		extra_instantiator: &Instantiator<'model>,
		arg_asts: I,
	) -> Option<&'model [&'model Expr<'model>]> {
		let instantiator = Instantiator::of_inst_method(inst_method).combine(extra_instantiator);
		self.check_arguments(loc, inst_method.method_decl, &instantiator, arg_asts)
	}

	fn check_arguments<'ast, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		method_decl: MethodOrImplOrAbstract<'model>,
		instantiator: &Instantiator<'model>,
		arg_asts: I,
	) -> Option<&'model [&'model Expr<'model>]> {
		let parameters = method_decl.parameters();
		if arg_asts.len() == parameters.len() {
			Some(
				self.ctx
					.arena
					.map(parameters.zip(arg_asts), |(parameter, arg_ast)| {
						let ty = self.instantiate_ty(&parameter.ty, instantiator);
						self.check_subtype(&ty, arg_ast)
					}),
			)
		} else {
			self.add_diagnostic(loc, Diag::ArgumentCountMismatch(method_decl, arg_asts.len()));
			None
		}
	}

	//mv
	fn instantiate_ty(&mut self, ty: &Ty<'model>, instantiator: &Instantiator<'model>) -> Ty<'model> {
		instantiate_ty(ty, instantiator, self.ctx.arena)
	}

	/*
	NOTE: Caller is responsible for checking that we can access this member's effect!
	If this returns None, we've already handled the error reporting, so just call handleBogus.
	*/
	fn get_member_of_inst_class(
		&mut self,
		loc: Loc,
		inst_class: &InstClass<'model>,
		member_name: Sym,
	) -> Option<InstMember<'model>> {
		let res = try_get_member_of_inst_class(inst_class, member_name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(inst_class.class, member_name))
		}
		res
	}

	//TODO:inline
	fn get_own_slot<'expected>(
		&mut self,
		expected: Expected<'expected, 'model>,
		loc: Loc,
		slot: &'model SlotDeclaration<'model>,
		instantiator: Instantiator<'model>,
	) -> Handled<'model> {
		if self.is_static {
			self.add_diagnostic(loc, Diag::CantAccessSlotFromStaticMethod(Up(slot)));
			return self.bogus(loc)
		}

		if slot.mutable && !self.self_effect.can_get() {
			self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(Up(slot)))
		}

		let slot_ty = instantiate_and_narrow_effects(
			self.self_effect,
			&slot.ty,
			&instantiator,
			loc,
			&mut self.ctx.diags,
			self.ctx.arena,
		);
		self.handle(expected, loc, slot_ty, ExprData::GetMySlot(Up(slot)))
	}

	fn handle<'expected>(&mut self, expected: Expected<'expected, 'model>, loc: Loc, ty: Ty<'model>, data: ExprData<'model>) -> Handled<'model> {
		match expected {
			Expected::Return(expected_ty) | Expected::SubTypeOf(expected_ty) => self.check_ty(expected_ty, loc, ty, data),
			Expected::Infer(inferred_ty_cell) => {
				let inferred_ty = unsafe { inferred_ty_cell.get().as_mut().unwrap() };
				let new_inferred_ty = match *inferred_ty {
					Some(ref last_inferred_ty) => self.get_compatible_ty(loc, last_inferred_ty, &ty),
					None => ty.clone(),
				};
				*inferred_ty = Some(new_inferred_ty);
				Handled(self.expr(loc, ty, data))
			}
		}
	}

	fn check_ty(&mut self, expected_ty: &Ty<'model>, loc: Loc, actual_ty: Ty<'model>, data: ExprData<'model>) -> Handled<'model> {
		if is_assignable(expected_ty, &actual_ty, self.ctx.arena) {
			Handled(self.expr(loc, actual_ty, data))
		} else {
			self.add_diagnostic(
				loc,
				Diag::NotAssignable { expected: expected_ty.clone(), actual: actual_ty.clone() },
			);
			let inner = self.expr(loc, actual_ty, data);
			Handled(self.expr(loc, expected_ty.clone(), ExprData::BogusCast(inner)))
		}
	}

	fn get_compatible_ty(&mut self, loc: Loc, a: &Ty<'model>, b: &Ty<'model>) -> Ty<'model> {
		common_ty(a, b).unwrap_or_else(|| {
			self.add_diagnostic(loc, Diag::CantCombineTypes(a.clone(), b.clone()));
			Ty::Bogus
		})
	}

	//mv
	fn instantiate_return_ty(&mut self, inst_method: &InstMethod<'model>) -> Ty<'model> {
		self.instantiate_ty(inst_method.method_decl.return_ty(), &Instantiator::of_inst_method(inst_method))
	}

	fn instantiate_return_ty_with_extra_instantiator(
		&mut self,
		inst_method: &InstMethod<'model>,
		instantiator: &Instantiator<'model>,
	) -> Ty<'model> {
		self.instantiate_ty(
			inst_method.method_decl.return_ty(),
			&Instantiator::of_inst_method(inst_method).combine(instantiator),
		)
	}

	fn expr(&self, loc: Loc, ty: Ty<'model>, data: ExprData<'model>) -> &'model Expr<'model> {
		self.ctx.arena <- Expr { loc, ty, data }
	}

	fn bogus(&self, loc: Loc) -> Handled<'model> {
		Handled(self.ctx.arena <- Expr { loc, ty: Ty::Bogus, data: ExprData::Bogus })
	}
}

//TODO:NEATER
struct Handled<'model>(&'model Expr<'model>);
