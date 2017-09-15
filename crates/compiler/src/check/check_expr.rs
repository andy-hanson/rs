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
use model::method::{InstMethod, MethodOrAbstract, MethodOrImpl, Parameter};
use model::ty::{InstCls, Ty};

use super::class_utils::{try_get_member_of_inst_cls, InstMember};
use super::ctx::Ctx;
use super::expected::Expected;
use super::instantiator::Instantiator;
use super::type_utils::{common_ty, instantiate_and_narrow_effects, instantiate_ty, is_assignable};

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
	ectx.check_return(return_ty, body)
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

	fn check_return<'ast>(&mut self, ty: Ty<'model>, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(&mut Expected::Return(ty), a)
	}

	//TODO: this is always called immediately after instantiate_ty, combine?
	fn check_subtype<'ast>(&mut self, ty: Ty<'model>, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(&mut Expected::SubTypeOf(ty), a)
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
		self.check_expr(&mut Expected::Infer(None), a)
	}

	fn check_expr<'ast>(
		&mut self,
		e: &mut Expected<'model>,
		a: &'ast ast::Expr<'ast>,
	) -> &'model Expr<'model> {
		self.check_expr_worker(e, a).0
	}

	fn check_expr_worker<'ast>(
		&mut self,
		e: &mut Expected<'model>,
		&ast::Expr(loc, ref ast_data): &'ast ast::Expr<'ast>,
	) -> Handled<'model> {
		match *ast_data {
			ast::ExprData::Access(name) => {
				if let Some(local) = self.try_find_local(name) {
					return self.handle(e, loc, ExprData::AccessLocal(Up(local)))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = self.instantiate_ty(&param.ty, self.method_instantiator);
					return self.handle(e, loc, ExprData::AccessParameter(Up(param), ty))
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
			ast::ExprData::StaticAccess(_, _) | ast::ExprData::TypeArguments(_) => {
				self.add_diagnostic(loc, Diag::MethodUsedAsValue);
				self.bogus(loc)
			}
			ast::ExprData::OperatorCall(&ast::OperatorCallData(ref left, operator, ref right)) => {
				let ty_args = todo(); //List::empty(); // No way to provide these to an operator call.
				self.call_method(e, loc, left, operator, ty_args, OptionIter(Some(right)))
			}
			ast::ExprData::Call(&ast::CallData(ref target, args)) =>
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
				self.handle(e, loc, ExprData::Recur(method_or_impl, args))
			}
			ast::ExprData::New(&ast::NewData(ty_arg_asts, arg_asts)) => {
				let slots = match *self.ctx.current_class.head {
					ClassHead::Slots(_, slots) => slots,
					_ => {
						self.add_diagnostic(loc, Diag::NewInvalid(Up(self.ctx.current_class)));
						return self.bogus(loc)
					}
				};
				if arg_asts.len() != slots.len() {
					self.add_diagnostic(
						loc,
						Diag::NewArgumentCountMismatch {
							class: Up(self.ctx.current_class),
							n_slots: slots.len(),
							n_arguments: arg_asts.len(),
						},
					);
					return self.bogus(loc)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len() {
					unimplemented!()
				}

				let inst_cls = unwrap_or_return!(
					self.ctx
						.instantiate_class(self.ctx.current_class, ty_arg_asts),
					self.bogus(loc)
				);
				let instantiator = Instantiator::of_inst_cls(&inst_cls);
				let args = self.ctx.arena.map(slots.zip(arg_asts), |(slot, arg)| {
					let ty = self.instantiate_ty(&slot.ty, &instantiator);
					self.check_subtype(ty, arg)
				});
				let ty = Ty::Plain(Effect::MAX, inst_cls);
				self.handle(e, loc, ExprData::New(ty, args))
			}
			ast::ExprData::ArrayLiteral(&ast::ArrayLiteralData(ref element_ty, args)) => {
				unused!(element_ty, args);
				unimplemented!()
			}
			ast::ExprData::GetProperty(&ast::GetPropertyData(ref target_ast, property_name)) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match *target.ty() {
					Ty::Bogus => return self.bogus(loc),
					Ty::Plain(target_effect, ref target_class) => {
						let InstMember(member_decl, instantiator) =
							//TODO: just get_slot_of_inst_cls
							unwrap_or_return!(
								self.get_member_of_inst_cls(target.loc(), target_class, property_name),
								self.bogus(loc));
						let slot = match member_decl {
							MemberDeclaration::Slot(s) => s,
							_ => {
								self.add_diagnostic(target.loc(), Diag::MethodUsedAsValue);
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
				self.handle(e, loc, ExprData::GetSlot(target, Up(slot), slot_ty))
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

				let ty = self.instantiate_ty(&slot.ty, &instantiator);
				let value = self.ctx.arena <- self.check_subtype(ty, value_ast);
				self.handle(e, loc, ExprData::SetSlot(Up(slot), value))
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
								self.ctx.arena <- Local { loc: pattern_loc, ty: value.ty().clone(), name };
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
				Handled(self.expr(loc, ExprData::Let(pattern, value, then)))
			}
			ast::ExprData::Seq(&ast::SeqData(ref first_ast, ref then_ast)) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(e, then_ast);
				// 'expected' was handled in 'then'
				Handled(self.expr(loc, ExprData::Seq(first, then)))
			}
			ast::ExprData::LiteralPass => unimplemented!(),
			ast::ExprData::LiteralBool(_) => unimplemented!(),
			ast::ExprData::LiteralNat(_) => unimplemented!(),
			ast::ExprData::LiteralInt(_) => unimplemented!(),
			ast::ExprData::LiteralFloat(_) => unimplemented!(),
			ast::ExprData::LiteralString(_) => unimplemented!(),
			ast::ExprData::SelfExpr => {
				/*
				Create an InstCls for the current class that just maps type parameters to theirselves.
				For example:
					class Foo[T]
						Foo[T] get-self()
							self || This is of type Foo[T] where T is the same as the type parameter on Foo.

					fun use-it(Foo[Int] foo)
                        || The return type has the same T,
						|| so it will be instantiated to return Foo[Int].
						foo.get-self()
				*/
				let expr = ExprData::SelfExpr(Ty::Plain(self.self_effect, self.current_inst_cls()));
				self.handle(e, loc, expr)
			}
			ast::ExprData::IfElse(&ast::IfElseData(ref test_ast, ref then_ast, ref else_ast)) => {
				let test = self.check_bool(test_ast);
				let then = self.check_expr(e, then_ast);
				let elze = self.check_expr(e, else_ast);
				let ifelse = ExprData::IfElse { test, then, elze, ty: e.inferred_ty().clone() };
				// 'expected' was handled in both 'then' and 'else'.
				Handled(self.expr(loc, ifelse))
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
				// 'expected' handled in each case result and in 'elze'.
				Handled(self.expr(loc, ExprData::WhenTest(cases, elze, e.inferred_ty().clone())))
			}
			ast::ExprData::Assert(asserted_ast) => {
				let asserted = self.check_bool(asserted_ast);
				self.handle( e, loc, ExprData::Assert(asserted))
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
				let try = ExprData::Try { body, catch, finally, ty: e.inferred_ty().clone() };
				// 'expected' handled when checking 'do' and 'catch'
				Handled(self.expr(loc, try))
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

	fn check_call_ast_worker<'ast>(
		&mut self,
		expected: &mut Expected<'model>,
		loc: Loc,
		target: &'ast ast::Expr<'ast>,
		args: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let &ast::Expr(target_loc, ref target_data) = target;
		let (&ast::Expr(real_target_loc, ref real_target_data), ty_arg_asts) = match *target_data {
			ast::ExprData::TypeArguments(&ast::TypeArgumentsData(ref real_target, ty_arg_asts)) =>
				(real_target, ty_arg_asts),
			_ => (target, List::EMPTY),
		};
		match *real_target_data {
			ast::ExprData::StaticAccess(class_name, static_method_name) => {
				let cls = unwrap_or_return!(
					self.ctx
						.access_class_declaration_or_add_diagnostic(real_target_loc, class_name),
					self.bogus(target_loc)
				);
				self.call_static_method(expected, loc, cls, static_method_name, ty_arg_asts, args)
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

	fn call_static_method<'ast>(
		&mut self,
		expected: &mut Expected<'model>,
		loc: Loc,
		cls: &'model ClassDeclaration<'model>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let method_decl = unwrap_or_return!(cls.find_static_method(method_name), {
			self.add_diagnostic(loc, Diag::StaticMethodNotFound(Up(cls), method_name));
			self.bogus(loc)
		});

		let inst_method = unwrap_or_return!(
			self.ctx
				.instantiate_method(MethodOrAbstract::Method(Up(method_decl)), ty_arg_asts),
			self.bogus(loc)
		);

		// No need to check selfEffect, because this is a static method.
		// Static methods can't look at their class' type arguments
		let args = unwrap_or_return!(
			self.check_call_arguments(loc, inst_method, &Instantiator::NIL, arg_asts),
			self.bogus(loc)
		);

		let ty = self.instantiate_return_ty(inst_method);
		self.handle(expected, loc, ExprData::StaticMethodCall(inst_method, args, ty))
	}

	//mv
	fn current_inst_cls(&mut self) -> InstCls<'model> {
		InstCls::generic_self_reference(self.ctx.current_class, self.ctx.arena)
	}

	fn call_own_method<'ast>(
		&mut self,
		expected: &mut Expected<'model>,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		// Note: InstCls is still relevent here:
		// Even if 'self' is not an inst, in a superclass we will fill in type parameters.
		let current_inst_cls = self.current_inst_cls();
		let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
			self.get_member_of_inst_cls(loc, &current_inst_cls, method_name),
			self.bogus(loc)
		);

		//TODO: helper fn for converting member -> method
		let method_decl = match member_decl {
			MemberDeclaration::Method(m) => MethodOrAbstract::Method(Up(m)),
			MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(Up(a)),
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
			ExprData::StaticMethodCall(inst_method, args, ty)
		} else {
			if self.is_static {
				self.add_diagnostic(loc, Diag::CantCallInstanceMethodFromStaticMethod(method_decl));
				return self.bogus(loc)
			}

			let target_effect = self.self_effect;
			if !target_effect.contains(method_decl.self_effect()) {
				self.add_diagnostic(loc, Diag::IllegalSelfEffect { target_effect, method: method_decl })
			}

			ExprData::MyInstanceMethodCall(inst_method, args, ty)
		};

		self.handle(expected, loc, expr)
	}

	fn call_method<'ast, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		expected: &mut Expected<'model>,
		loc: Loc,
		target_ast: &'ast ast::Expr<'ast>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: I,
	) -> Handled<'model> {
		let target = self.check_infer(target_ast);
		let (inst_method, args, ty) = match *target.ty() {
			Ty::Bogus =>
				// Already issued an error, don't need another.
				return self.bogus(loc),
			Ty::Plain(target_effect, ref target_inst_cls) => {
				let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
					self.get_member_of_inst_cls(loc, target_inst_cls, method_name),
					self.bogus(loc));

				let method = match member_decl {
					MemberDeclaration::Method(m) => MethodOrAbstract::Method(Up(m)),
					MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(Up(a)),
					_ => {
						self.add_diagnostic(loc, Diag::CallsNonMethod);
						return self.bogus(loc)
					}
				};

				if let MethodOrAbstract::Method(m) = method {
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
		let e = ExprData::InstanceMethodCall(target, inst_method, args, ty);
		self.handle(expected, loc, e)
	}

	fn check_call_arguments<'ast, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		inst_method: &'model InstMethod<'model>,
		extra_instantiator: &Instantiator<'model>,
		arg_asts: I,
	) -> Option<&'model [&'model Expr<'model>]> {
		let instantiator = Instantiator::of_inst_method(inst_method).combine(extra_instantiator);
		self.check_arguments(loc, inst_method.0, &instantiator, arg_asts)
	}

	fn check_arguments<'ast, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		method_decl: MethodOrAbstract<'model>,
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
						self.check_subtype(ty, arg_ast)
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
	fn get_member_of_inst_cls(
		&mut self,
		loc: Loc,
		inst_cls: &InstCls<'model>,
		member_name: Sym,
	) -> Option<InstMember<'model>> {
		let res = try_get_member_of_inst_cls(inst_cls, member_name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(Up(inst_cls.0), member_name))
		}
		res
	}

	//TODO:inline
	fn get_own_slot(
		&mut self,
		expected: &mut Expected<'model>,
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
		self.handle(expected, loc, ExprData::GetMySlot(Up(slot), slot_ty))
	}

	fn handle(&mut self, expected: &mut Expected<'model>, loc: Loc, e: ExprData<'model>) -> Handled<'model> {
		match *expected {
			Expected::Return(ref ty) | Expected::SubTypeOf(ref ty) => self.check_ty(ty, loc, e),
			Expected::Infer(ref mut inferred_ty) => {
				let expr = self.expr(loc, e);
				let expr_ty = expr.ty();
				let new_inferred_ty = match *inferred_ty {
					Some(ref last_inferred_ty) => self.get_compatible_ty(loc, last_inferred_ty, expr_ty),
					None => expr_ty.clone(),
				};
				*inferred_ty = Some(new_inferred_ty);
				Handled(expr)
			}
		}
	}

	fn check_ty(&mut self, expected_ty: &Ty<'model>, loc: Loc, e: ExprData<'model>) -> Handled<'model> {
		if is_assignable(expected_ty, e.ty(), self.ctx.arena) {
			Handled(self.expr(loc, e))
		} else {
			self.add_diagnostic(
				loc,
				Diag::NotAssignable { expected: expected_ty.clone(), actual: e.ty().clone() },
			);
			let inner = self.alloc(Expr(loc, e));
			Handled(self.expr(loc, ExprData::BogusCast(expected_ty.clone(), inner)))
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
		self.instantiate_ty(inst_method.0.return_ty(), &Instantiator::of_inst_method(inst_method))
	}

	fn instantiate_return_ty_with_extra_instantiator(
		&mut self,
		inst_method: &InstMethod<'model>,
		instantiator: &Instantiator<'model>,
	) -> Ty<'model> {
		self.instantiate_ty(
			inst_method.0.return_ty(),
			&Instantiator::of_inst_method(inst_method).combine(instantiator),
		)
	}

	fn expr(&self, loc: Loc, data: ExprData<'model>) -> &'model Expr<'model> {
		self.ctx.arena <- Expr(loc, data)
	}

	fn bogus(&self, loc: Loc) -> Handled<'model> {
		Handled(self.ctx.arena <- Expr(loc, ExprData::Bogus))
	}
}

//TODO:NEATER
struct Handled<'model>(&'model Expr<'model>);
