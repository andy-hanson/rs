use util::arena::List;
use util::arr::SliceOps;
use util::loc::Loc;
use util::sym::Sym;
use util::up::Up;
use util::utils::todo;

use model::class::{ClassDeclaration, ClassHead, MemberDeclaration, SlotDeclaration};
use model::diag::Diag;
use model::effect::Effect;
use model::expr::{Case, Catch, Expr, ExprData, Local, Pattern};
use model::method::{InstMethod, MethodOrAbstract, MethodOrImpl, Parameter};
use model::ty::{InstCls, Ty};

use super::super::parse::ast;

use super::class_utils::{try_get_member_of_inst_cls, InstMember};
use super::ctx::Ctx;
use super::instantiator::Instantiator;
use super::type_utils::{common_type, instantiate_and_narrow_effects, instantiate_type, is_assignable};

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
	let return_ty = ectx.instantiate_type(&signature.return_ty, method_instantiator);
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

	fn check_subtype<'ast>(&mut self, ty: Ty<'model>, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(&mut Expected::SubTypeOf(ty), a)
	}

	fn check_void<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		let void = self.ctx.void();
		self.check_subtype(void, a)
	}

	fn check_bool<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		let bool = self.ctx.bool();
		self.check_subtype(bool, a)
	}

	fn check_infer<'ast>(&mut self, a: &'ast ast::Expr<'ast>) -> &'model Expr<'model> {
		self.check_expr(&mut Expected::Infer(None), a)
	}

	fn check_expr<'ast>(
		&mut self,
		mut e: &mut Expected<'model>,
		a: &'ast ast::Expr<'ast>,
	) -> &'model Expr<'model> {
		self.check_expr_worker(&mut e, a).0
	}

	fn check_expr_worker<'ast>(
		&mut self,
		mut e: &mut Expected<'model>,
		&ast::Expr(loc, ref ast_data): &'ast ast::Expr<'ast>,
	) -> Handled<'model> {
		match *ast_data {
			ast::ExprData::Access(name) => {
				if let Some(local) = self.try_find_local(name) {
					return self.handle(&mut e, loc, ExprData::AccessLocal(Up(local)))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = self.instantiate_type(&param.ty, self.method_instantiator);
					return self.handle(&mut e, loc, ExprData::AccessParameter(Up(param), ty))
				}

				if let Some(InstMember(decl, instantiator)) = self.ctx.get_own_member_or_add_diagnostic(loc, name) {
					return match decl {
						MemberDeclaration::Slot(slot) =>
							self.get_own_slot(&mut e, loc, slot, instantiator),
						MemberDeclaration::Method(_) | MemberDeclaration::AbstractMethod(_) =>
							unimplemented!(), //diagnostic
					}
				}

				self.bogus(loc)
			}
			ast::ExprData::StaticAccess(_, _) | ast::ExprData::TypeArguments(_, _) => {
				self.add_diagnostic(loc, Diag::MethodUsedAsValue);
				self.bogus(loc)
			}
			ast::ExprData::OperatorCall(left, operator, right) => {
				let ty_args = todo();//List::empty(); // No way to provide these to an operator call.
				self.call_method(&mut e, loc, left, operator, ty_args, Some(right).into_iter())
			}
			ast::ExprData::Call(target, ref args) =>
				self.check_call_ast_worker(&mut e, loc, target, args),
			ast::ExprData::Recur(ref arg_asts) => {
				if !e.in_tail_call_position() {
					self.add_diagnostic(loc, Diag::NotATailCall)
				}
				// For recursion, need to do substitution in case we are
				// implementing an abstract method where the superclass took type arguments.
				let method_or_impl = self.method_or_impl;
				let moa = method_or_impl.method_or_abstract();
				let inst = self.method_instantiator;
				let args = unwrap_or_return!(
					self.check_arguments(loc, moa, inst, arg_asts.iter().cloned()),
					self.bogus(loc));
				self.handle(&mut e, loc, ExprData::Recur(method_or_impl, args))
			}
			ast::ExprData::New(ref ty_arg_asts, ref arg_asts) => {
				let slots = match *self.ctx.current_class.head {
					ClassHead::Slots(_, slots) => slots,
					_ => {
						self.add_diagnostic(loc, Diag::NewInvalid(Up(self.ctx.current_class)));
						return self.bogus(loc)
					}
				};
				if arg_asts.len != slots.len() {
					self.add_diagnostic(loc, Diag::NewArgumentCountMismatch {
						class: Up(self.ctx.current_class),
						n_slots: slots.len(),
						n_arguments: arg_asts.len,
					});
					return self.bogus(loc)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len {
					unimplemented!()
				}

				let inst_cls = unwrap_or_return!(
					self.ctx.instantiate_class(self.ctx.current_class, ty_arg_asts),
					self.bogus(loc));
				let instantiator = Instantiator::of_inst_cls(&inst_cls);
				let args = slots.zip(arg_asts.iter(), self.ctx.arena, |slot, arg| {
					let ty = self.instantiate_type(&slot.ty, &instantiator);
					self.check_subtype(ty, arg)
				});
				let ty = Ty::Plain(Effect::MAX, inst_cls);
				self.handle(&mut e, loc, ExprData::New(ty, args))
			}
			ast::ExprData::ArrayLiteral(ref element_ty, ref args) => {
				unused!(element_ty, args);
				unimplemented!()
			}
			ast::ExprData::GetProperty(target_ast, property_name) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match *target.ty() {
					Ty::Bogus =>
						return self.bogus(loc),
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
							target_effect, &slot.ty, &instantiator, loc, &self.ctx.diags, self.ctx.arena);
						(slot, slot_ty)
					},
					Ty::Param(_) =>
						unimplemented!()
				};
				self.handle(&mut e, loc, ExprData::GetSlot(target, Up(slot), slot_ty))
			}
			ast::ExprData::SetProperty(property_name, value_ast) => {
				let InstMember(member_decl, instantiator) =
					unwrap_or_return!(self.ctx.get_own_member_or_add_diagnostic(loc, property_name), self.bogus(loc));
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
					self.add_diagnostic(loc, Diag::MissingEffectToSetSlot {
						allowed_effect,
						slot: Up(slot)
					})
				}

				let ty = self.instantiate_type(&slot.ty, &instantiator);
				let value = self.ctx.arena <- self.check_subtype(ty, value_ast);
				self.handle(&mut e, loc, ExprData::SetSlot(Up(slot), value))
			}
			ast::ExprData::LetInProgress(_, _) =>
				unreachable!(),
			ast::ExprData::Let(pattern_ast, value_ast, then_ast) => {
				let value = self.check_infer(value_ast);
				let (pattern, n_added) = {//self.start_check_pattern(value.ty(), pattern_ast);
					let &ast::Pattern(pattern_loc, ref pattern_data) = pattern_ast;
					match *pattern_data {
						ast::PatternData::Ignore =>
							(Pattern::Ignore, 0),
						ast::PatternData::Single(name) => {
							let local = self.ctx.arena <- Local { loc: pattern_loc, ty: value.ty().clone(), name };
							self.add_to_scope(local);
							(Pattern::Single(local), 1)
						}
						ast::PatternData::Destruct(_) => {
							unimplemented!()
						}
					}
				};
				let then = self.check_expr(&mut e, then_ast);
				for _ in 0..n_added {
					self.pop_from_scope()
				}
				// 'expected' was handled in 'then'
				Handled(self.expr(loc, ExprData::Let(pattern, value, then)))
			}
			ast::ExprData::Seq(first_ast, then_ast) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(&mut e, then_ast);
				// 'expected' was handled in 'then'
				Handled(self.expr(loc, ExprData::Seq(first, then)))
			}
			ast::ExprData::Literal(ref value) => {
				unused!(value);
				unimplemented!()
			}
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
				self.handle(&mut e, loc, expr)
			}
			ast::ExprData::IfElse(test_ast, then_ast, else_ast) => {
				let test = self.check_bool(test_ast);
				let then = self.check_expr(&mut e, then_ast);
				let elze = self.check_expr(&mut e, else_ast);
                let ifelse = ExprData::IfElse {
					test,
					then,
					elze,
					ty: e.inferred_ty().clone(),
				};
				// 'expected' was handled in both 'then' and 'else'.
				Handled(self.expr(loc, ifelse))
			}
			ast::ExprData::WhenTest(ref case_asts, else_ast) => {
				let cases = case_asts.map(self.ctx.arena, |&ast::Case(case_loc, test_ast, result_ast)| {
					let test = self.check_bool(test_ast);
					let result = self.check_expr(&mut e, result_ast);
					Case(case_loc, test, result)
				});
				let elze = self.check_expr(&mut e, else_ast);
				// 'expected' handled in each case result and in 'elze'.
				Handled(self.expr(loc, ExprData::WhenTest(cases, elze, e.inferred_ty().clone())))
			}
			ast::ExprData::Assert(asserted_ast) => {
				let asserted = self.check_bool(asserted_ast);
				self.handle(&mut e, loc, ExprData::Assert(asserted))
			}
			ast::ExprData::Try(do_ast, ref op_catch_ast, op_finally_ast) => {
				let body = self.check_expr(&mut e, do_ast);
				let catch = op_catch_ast.as_ref().map(|catch| {
                    let &ast::Catch {
						loc: catch_loc,
						exception_type: ref exception_type_ast,
						exception_name_loc,
						exception_name,
						then: then_ast
					} = catch;
					let exception_ty = self.ctx.get_ty(exception_type_ast);
					let caught = self.ctx.arena <- Local {
                        loc: exception_name_loc,
                        ty: exception_ty,
                        name: exception_name
                    };
					self.add_to_scope(caught);
					let then = self.check_expr(&mut e, then_ast);
					self.pop_from_scope();
					Catch(catch_loc, caught, then)
				});
				let finally = op_finally_ast.as_ref().map(|f| self.check_void(f));
				let try = ExprData::Try { body, catch, finally, ty: e.inferred_ty().clone() };
				// 'expected' handled when checking 'do' and 'catch'
				Handled(self.expr(loc, try))
			}
			ast::ExprData::For(_, _, _) => //(local_name, ref looper, ref body) =>
				unimplemented!(),
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
		if let Some(param) = self.current_parameters.find(|p| p.name == local.name) {
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
		mut expected: &mut Expected<'model>,
		loc: Loc,
		target: &'ast ast::Expr<'ast>,
		args: &'ast List<&'ast ast::Expr<'ast>>,
	) -> Handled<'model> {
		let &ast::Expr(target_loc, ref target_data) = target;
		let empty_list = todo(); //List::empty(); //TODO:PERF
		let (&ast::Expr(real_target_loc, ref real_target_data), ty_arg_asts) = match *target_data {
			ast::ExprData::TypeArguments(real_target, ref ty_arg_asts) => {
				//TODO:SIMPLIFY
				let a: &ast::Expr = real_target;
				let b: &List<ast::Ty> = ty_arg_asts;
				(a, b)
			}
			_ => {
				let a: &ast::Expr = target;
				let b: &List<ast::Ty> = empty_list;
				(a, b)
			}
		};
		match *real_target_data {
			ast::ExprData::StaticAccess(class_name, static_method_name) => {
				let cls = unwrap_or_return!(
					self.ctx
						.access_class_declaration_or_add_diagnostic(real_target_loc, class_name),
					self.bogus(target_loc)
				);
				self.call_static_method(&mut expected, loc, cls, static_method_name, ty_arg_asts, args)
			}
			ast::ExprData::GetProperty(property_target, property_name) =>
				self.call_method(
					&mut expected,
					loc,
					property_target,
					property_name,
					ty_arg_asts,
					args.iter().cloned(),
				),
			ast::ExprData::Access(name) => self.call_own_method(&mut expected, loc, name, ty_arg_asts, args),
			_ => {
				self.add_diagnostic(loc, Diag::CallsNonMethod);
				self.bogus(target_loc)
			}
		}
	}

	fn call_static_method<'ast>(
		&mut self,
		mut expected: &mut Expected<'model>,
		loc: Loc,
		cls: &'model ClassDeclaration<'model>,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
		arg_asts: &'ast List<'ast, &'ast ast::Expr<'ast>>,
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
			self.check_call_arguments(loc, inst_method, &Instantiator::NIL, arg_asts.iter().cloned()),
			self.bogus(loc)
		);

		let ty = self.instantiate_return_type(inst_method);
		self.handle(&mut expected, loc, ExprData::StaticMethodCall(inst_method, args, ty))
	}

	//mv
	fn current_inst_cls(&mut self) -> InstCls<'model> {
		InstCls::generic_self_reference(self.ctx.current_class, self.ctx.arena)
	}

	fn call_own_method<'ast>(
		&mut self,
		mut expected: &mut Expected<'model>,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
		arg_asts: &'ast List<'ast, &'ast ast::Expr<'ast>>,
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
			self.check_call_arguments(loc, inst_method, &member_instantiator, arg_asts.iter().cloned()),
			self.bogus(loc)
		);

		let ty = self.instantiate_return_type(inst_method);

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

		self.handle(&mut expected, loc, expr)
	}

	fn call_method<'ast, I: Iterator<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		mut expected: &mut Expected<'model>,
		loc: Loc,
		target_ast: &'ast ast::Expr<'ast>,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
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
					self.check_call_arguments(loc, inst_method, &member_instantiator, arg_asts.into_iter()),
					self.bogus(loc));

				let ty = self.instantiate_return_type_with_extra_instantiator(inst_method, &member_instantiator);
				(inst_method, args, ty)
			}
			Ty::Param(_) =>
				unimplemented!()
		};
		let e = ExprData::InstanceMethodCall(target, inst_method, args, ty);
		self.handle(&mut expected, loc, e)
	}

	fn check_call_arguments<'ast, I: Iterator<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		inst_method: &'model InstMethod<'model>,
		extra_instantiator: &Instantiator<'model>,
		arg_asts: I,
	) -> Option<&'model [&'model Expr<'model>]> {
		let instantiator = Instantiator::of_inst_method(inst_method).combine(extra_instantiator);
		self.check_arguments(loc, inst_method.0, &instantiator, arg_asts)
	}

	fn check_arguments<'ast, I: Iterator<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		loc: Loc,
		method_decl: MethodOrAbstract<'model>,
		instantiator: &Instantiator<'model>,
		arg_asts: I,
	) -> Option<&'model [&'model Expr<'model>]> {
		let parameters = method_decl.parameters();
		match parameters.try_zip(arg_asts, self.ctx.arena, |parameter, arg_ast| {
			let ty = self.instantiate_type(&parameter.ty, instantiator);
			self.check_subtype(ty, arg_ast)
		}) {
			Ok(res) => Some(res),
			Err((_, n_args)) => {
				self.add_diagnostic(loc, Diag::ArgumentCountMismatch(method_decl, n_args));
				None
			}
		}
	}

	//mv
	fn instantiate_type(&mut self, ty: &Ty<'model>, instantiator: &Instantiator<'model>) -> Ty<'model> {
		instantiate_type(ty, instantiator, self.ctx.arena)
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
		mut expected: &mut Expected<'model>,
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
			&self.ctx.diags,
			self.ctx.arena,
		);
		self.handle(&mut expected, loc, ExprData::GetMySlot(Up(slot), slot_ty))
	}

	fn handle(&mut self, expected: &mut Expected<'model>, loc: Loc, e: ExprData<'model>) -> Handled<'model> {
		match *expected {
			Expected::Return(ref ty) | Expected::SubTypeOf(ref ty) => self.check_ty(ty, loc, e),
			Expected::Infer(ref mut inferred_ty) => {
				let expr = self.expr(loc, e);
				let expr_ty = expr.ty();
				let new_inferred_ty = match *inferred_ty {
					Some(ref mut last_inferred_ty) =>
						self.get_compatible_type(loc, last_inferred_ty, expr_ty),
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

	fn get_compatible_type(&mut self, loc: Loc, a: &Ty<'model>, b: &Ty<'model>) -> Ty<'model> {
		common_type(a, b).unwrap_or_else(|| {
			self.add_diagnostic(loc, Diag::CantCombineTypes(a.clone(), b.clone()));
			Ty::Bogus
		})
	}

	//mv
	fn instantiate_return_type(&mut self, inst_method: &InstMethod<'model>) -> Ty<'model> {
		self.instantiate_type(inst_method.0.return_ty(), &Instantiator::of_inst_method(inst_method))
	}

	fn instantiate_return_type_with_extra_instantiator(
		&mut self,
		inst_method: &InstMethod<'model>,
		instantiator: &Instantiator<'model>,
	) -> Ty<'model> {
		self.instantiate_type(
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

enum Expected<'model> {
	/* Return is identical to SubTypeOf, but marks that we're in a tail call position. */
	Return(Ty<'model>),
	SubTypeOf(Ty<'model>),
	/* Expected should always be passed by `&mut`, so that inferred types can be inserted here. */
	Infer(Option<Ty<'model>>),
}
impl<'model> Expected<'model> {
	fn inferred_ty(&self) -> &Ty<'model> {
		self.current_expected_ty().unwrap()
	}

	fn current_expected_ty(&self) -> Option<&Ty<'model>> {
		match *self {
			Expected::Return(ref ty) | Expected::SubTypeOf(ref ty) => Some(ty),
			Expected::Infer(ref ty_op) => ty_op.as_ref(),
		}
	}

	fn in_tail_call_position(&self) -> bool {
		match *self {
			Expected::Return(_) => true,
			_ => false,
		}
	}
}
