use std::cell::RefCell;

use util::arena::List;
use util::arr::{Arr, SliceOps};
use util::loc::Loc;
use util::ptr::{Own, Ptr};
use util::sym::Sym;
use util::utils::todo;

use super::super::super::model::class::{ClassDeclaration, ClassHead, MemberDeclaration, SlotDeclaration};
use super::super::super::model::diag::Diag;
use super::super::super::model::effect::Effect;
use super::super::super::model::expr::{Case, Catch, Expr, ExprData, Local, Pattern};
use super::super::super::model::method::{InstMethod, MethodOrAbstract, MethodOrImpl, Parameter};
use super::super::super::model::ty::{InstCls, Ty};

use super::super::parse::ast;

use super::class_utils::{try_get_member_of_inst_cls, InstMember};
use super::ctx::Ctx;
use super::instantiator::Instantiator;
use super::type_utils::{common_type, instantiate_and_narrow_effects, instantiate_type, is_assignable};

pub fn check_method_body<'ast>(
	ctx: &Ctx,
	method_or_impl: &MethodOrImpl,
	method_instantiator: &Instantiator,
	is_static: bool,
	body: &'ast ast::Expr<'ast>,
) -> Expr {
	let signature = method_or_impl.signature();
	let return_ty = instantiate_type(&signature.return_ty, method_instantiator);
	let ectx = CheckExprContext {
		ctx,
		method_or_impl,
		method_instantiator,
		is_static,
		self_effect: signature.self_effect,
		current_parameters: &signature.parameters,
		locals: RefCell::new(Vec::new()),
	};
	ectx.check_return(return_ty, body)
}

struct CheckExprContext<'a> {
	ctx: &'a Ctx<'a>,
	method_or_impl: &'a MethodOrImpl,
	method_instantiator: &'a Instantiator,
	is_static: bool,
	self_effect: Effect,
	current_parameters: &'a Arr<Own<Parameter>>,
	locals: RefCell<Vec<Ptr<Local>>>,
}
impl<'a> CheckExprContext<'a> {
	fn add_diagnostic(&self, loc: Loc, data: Diag) {
		self.ctx.add_diagnostic(loc, data)
	}

	fn check_return<'ast>(&self, ty: Ty, a: &'ast ast::Expr<'ast>) -> Expr {
		self.check_expr(&mut Expected::Return(ty), a)
	}

	fn check_subtype<'ast>(&self, ty: Ty, a: &'ast ast::Expr<'ast>) -> Expr {
		self.check_expr(&mut Expected::SubTypeOf(ty), a)
	}

	fn check_void<'ast>(&self, a: &'ast ast::Expr<'ast>) -> Expr {
		// Need to access builtins::void
		self.check_subtype(self.ctx.void(), a)
	}

	fn check_bool<'ast>(&self, a: &'ast ast::Expr<'ast>) -> Expr {
		self.check_subtype(self.ctx.bool(), a)
	}

	fn check_infer<'ast>(&self, a: &'ast ast::Expr<'ast>) -> Expr {
		self.check_expr(&mut Expected::Infer(None), a)
	}

	fn check_expr<'ast>(&self, mut e: &mut Expected, a: &'ast ast::Expr<'ast>) -> Expr {
		self.check_expr_worker(&mut e, a).0
	}

	fn check_expr_worker<'ast>(&self, mut e: &mut Expected, &ast::Expr(loc, ref ast_data): &'ast ast::Expr<'ast>) -> Handled {
		match *ast_data {
			ast::ExprData::Access(name) => {
				if let Some(local) = self.locals.borrow().iter().find(|l| l.name == name) {
					return self.handle(&mut e, loc, ExprData::AccessLocal(local.clone_ptr()))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = instantiate_type(&param.ty, self.method_instantiator);
					return self.handle(&mut e, loc, ExprData::AccessParameter(param.ptr(), ty))
				}

				if let Some(InstMember(decl, instantiator)) = self.ctx.get_own_member_or_add_diagnostic(loc, name) {
					return match decl {
						MemberDeclaration::Slot(slot) =>
							self.get_own_slot(&mut e, loc, slot, instantiator),
						MemberDeclaration::Method(_) | MemberDeclaration::AbstractMethod(_) =>
							unimplemented!(), //diagnostic
					}
				}

				bogus(loc)
			}
			ast::ExprData::StaticAccess(_, _) | ast::ExprData::TypeArguments(_, _) => {
				self.add_diagnostic(loc, Diag::DelegatesNotYetSupported);
				bogus(loc)
			}
			ast::ExprData::OperatorCall(ref left, operator, ref right) => {
				let ty_args = todo();//List::empty(); // No way to provide these to an operator call.
				self.call_method(&mut e, loc, left, operator, ty_args, Some(right).into_iter())
			}
			ast::ExprData::Call(ref target, ref args) =>
				self.check_call_ast_worker(&mut e, loc, target, args),
			ast::ExprData::Recur(ref arg_asts) => {
				if !e.in_tail_call_position() {
					self.add_diagnostic(loc, Diag::NotATailCall)
				}
				// For recursion, need to do substitution in case we are
				// implementing an abstract method where the superclass took type arguments.
				let moa = &self.method_or_impl.method_or_abstract();
				let inst = self.method_instantiator;
				let args = unwrap_or_return!(self.check_arguments(loc, moa, inst, arg_asts.iter()), bogus(loc));
				self.handle(&mut e, loc, ExprData::Recur(self.method_or_impl.copy(), args))
			}
			ast::ExprData::New(ref ty_arg_asts, ref arg_asts) => {
				let slots = match *self.ctx.current_class.head {
					ClassHead::Slots(_, ref slots) => slots,
					_ => {
						self.add_diagnostic(loc, Diag::NewInvalid(self.ctx.current_class.ptr()));
						return bogus(loc)
					}
				};
				if arg_asts.len != slots.len() {
					self.add_diagnostic(loc, Diag::NewArgumentCountMismatch {
						expected: slots.len(),
						actual: arg_asts.len,
					});
					return bogus(loc)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len {
					unimplemented!()
				}

				let inst_cls = unwrap_or_return!(
					self.ctx.instantiate_class(self.ctx.current_class.ptr(), ty_arg_asts),
					bogus(loc));
				let instantiator = Instantiator::of_inst_cls(&inst_cls);
				let args = slots.zip(arg_asts.iter(), |slot, arg|
					self.check_subtype(instantiate_type(&slot.ty, &instantiator), arg));
				let ty = Ty::Plain(Effect::MAX, inst_cls);
				self.handle(&mut e, loc, ExprData::New(ty, args))
			}
			ast::ExprData::ArrayLiteral(ref element_ty, ref args) => {
				unused!(element_ty, args);
				unimplemented!()
			}
			ast::ExprData::GetProperty(ref target_ast, property_name) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match *target.ty() {
					Ty::Bogus =>
						return bogus(loc),
					Ty::Plain(target_effect, ref target_class) => {
						let InstMember(member_decl, instantiator) =
							//TODO: just get_slot_of_inst_cls
							unwrap_or_return!(self.get_member_of_inst_cls(target.loc(), target_class, property_name), bogus(loc));
						let slot = match member_decl {
							MemberDeclaration::Slot(s) => s,
							_ => {
								self.add_diagnostic(target.loc(), Diag::DelegatesNotYetSupported);
								return bogus(loc)
							}
						};
						if slot.mutable && !target_effect.can_get() {
							self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(slot.clone_ptr()))
						}
						let slot_ty = instantiate_and_narrow_effects(
							target_effect, &slot.ty, &instantiator, loc, &mut self.ctx.borrow_diags());
						(slot, slot_ty)
					},
					Ty::Param(_) =>
						unimplemented!()
				};
				self.handle(&mut e, loc, ExprData::GetSlot(Box::new(target), slot, slot_ty))
			}
			ast::ExprData::SetProperty(property_name, ref value_ast) => {
				let InstMember(member_decl, instantiator) =
					unwrap_or_return!(self.ctx.get_own_member_or_add_diagnostic(loc, property_name), bogus(loc));
				let slot = match member_decl {
					MemberDeclaration::Slot(s) => s,
					_ => {
						self.add_diagnostic(loc, Diag::CantSetNonSlot(member_decl));
						return bogus(loc)
					}
				};
				if !slot.mutable {
					self.add_diagnostic(loc, Diag::SlotNotMutable(slot.clone_ptr()))
				}
				if !self.self_effect.can_set() {
					self.add_diagnostic(loc, Diag::MissingEffectToSetSlot(self.self_effect, slot.clone_ptr()))
				}

				let value = self.check_subtype(instantiate_type(&slot.ty, &instantiator), value_ast);
				self.handle(&mut e, loc, ExprData::SetSlot(slot, Box::new(value)))
			}
			ast::ExprData::LetInProgress(_, _) =>
				unreachable!(),
			ast::ExprData::Let(ref pattern_ast, ref value_ast, ref then_ast) => {
				let value = self.check_infer(value_ast);
				let (pattern, n_added) = {//self.start_check_pattern(value.ty(), pattern_ast);
					let &&ast::Pattern(pattern_loc, ref pattern_data) = pattern_ast;
					match *pattern_data {
						ast::PatternData::Ignore =>
							(Pattern::Ignore, 0),
						ast::PatternData::Single(name) => {
							let local = Own::new(Local { loc: pattern_loc, ty: value.ty().clone(), name });
							self.add_to_scope(local.ptr());
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
				Handled(Expr(loc, ExprData::Let(pattern, Box::new(value), Box::new(then))))
			}
			ast::ExprData::Seq(ref first_ast, ref then_ast) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(&mut e, then_ast);
				// 'expected' was handled in 'then'
				Handled(Expr(loc, ExprData::Seq(Box::new(first), Box::new(then))))
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
			ast::ExprData::IfElse(ref test_ast, ref then_ast, ref else_ast) => {
				let test = self.check_bool(test_ast);
				let then = self.check_expr(&mut e, then_ast);
				let elze = self.check_expr(&mut e, else_ast);
                let ifelse = ExprData::IfElse {
					test: Box::new(test),
					then: Box::new(then),
					elze: Box::new(elze),
					ty: e.inferred_ty().clone(),
				};
				// 'expected' was handled in both 'then' and 'else'.
				Handled(Expr(loc, ifelse))
			}
			ast::ExprData::WhenTest(ref case_asts, ref else_ast) => {
				let cases = case_asts.map(|&ast::Case(case_loc, ref test_ast, ref result_ast)| {
					let test = self.check_bool(test_ast);
					let result = self.check_expr(&mut e, result_ast);
					Case(case_loc, Box::new(test), Box::new(result))
				});
				let elze = self.check_expr(&mut e, else_ast);
				// 'expected' handled in each case result and in 'elze'.
				Handled(Expr(loc, ExprData::WhenTest(cases, Box::new(elze), e.inferred_ty().clone())))
			}
			ast::ExprData::Assert(ref asserted_ast) => {
				let asserted = self.check_bool(asserted_ast);
				self.handle(&mut e, loc, ExprData::Assert(Box::new(asserted)))
			}
			ast::ExprData::Try(ref do_ast, ref op_catch_ast, ref op_finally_ast) => {
				let body = self.check_expr(&mut e, do_ast);
				let catch = op_catch_ast.as_ref().map(|catch| {
                    let &ast::Catch {
						loc: catch_loc,
						exception_type: ref exception_type_ast,
						exception_name_loc,
						exception_name,
						then: ref then_ast
					} = catch;
					let exception_ty = self.ctx.get_ty(exception_type_ast);
					let caught = Own::new(Local {
                        loc: exception_name_loc,
                        ty: exception_ty,
                        name: exception_name
                    });
					self.add_to_scope(caught.ptr());
					let then = self.check_expr(&mut e, then_ast);
					self.pop_from_scope();
					Catch(catch_loc, caught, Box::new(then))
				});
				let finally = op_finally_ast.as_ref().map(|f| Box::new(self.check_void(f)));
				let try = ExprData::Try { body: Box::new(body), catch, finally, ty: e.inferred_ty().clone() };
				// 'expected' handled when checking 'do' and 'catch'
				Handled(Expr(loc, try))
			}
			ast::ExprData::For(_, _, _) => //(local_name, ref looper, ref body) =>
				unimplemented!(),
		}
	}

	//mv
	fn add_to_scope(&self, local: Ptr<Local>) {
		// It's important that we push even in the presence of errors, because we will always pop.
		if let Some(param) = self.current_parameters.find(|p| p.name == local.name) {
			self.add_diagnostic(local.loc, Diag::CantReassignParameter(param.ptr()))
		}
		if let Some(old_local) = self.locals.borrow().iter().find(|l| l.name == local.name) {
			self.add_diagnostic(local.loc, Diag::CantReassignLocal(old_local.clone_ptr()))
		}
		self.locals.borrow_mut().push(local)
	}
	fn pop_from_scope(&self) {
		let popped = self.locals.borrow_mut().pop();
		assert!(popped.is_some())
	}

	fn check_call_ast_worker<'ast>(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		target: &'ast ast::Expr<'ast>,
		args: &'ast List<&'ast ast::Expr<'ast>>,
	) -> Handled {
		let &ast::Expr(target_loc, ref target_data) = target;
		let empty_list = todo();//List::empty(); //TODO:PERF
		let (&ast::Expr(real_target_loc, ref real_target_data), ty_arg_asts) = match *target_data {
			ast::ExprData::TypeArguments(ref real_target, ref ty_arg_asts) => {
				//TODO:SIMPLIFY
				let a: &ast::Expr = real_target;
				let b: &List<ast::Ty> = ty_arg_asts;
				(a, b)
			}
			_ => {
				let a: &ast::Expr = target;
				let b: &List<ast::Ty> = empty_list;
				(a, b)
			},
		};
		match *real_target_data {
			ast::ExprData::StaticAccess(class_name, static_method_name) => {
				let cls = unwrap_or_return!(
					self.ctx
						.access_class_declaration_or_add_diagnostic(real_target_loc, class_name),
					bogus(target_loc)
				);
				self.call_static_method(&mut expected, loc, cls, static_method_name, ty_arg_asts, args)
			}
			ast::ExprData::GetProperty(ref property_target, property_name) =>
				self.call_method(&mut expected, loc, property_target, property_name, ty_arg_asts, args.iter()),
			ast::ExprData::Access(name) => self.call_own_method(&mut expected, loc, name, ty_arg_asts, args),
			_ => {
				self.add_diagnostic(loc, Diag::DelegatesNotYetSupported);
				bogus(target_loc)
			}
		}
	}

	fn call_static_method<'ast>(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		cls: Ptr<ClassDeclaration>,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
		arg_asts: &'ast List<'ast, &'ast ast::Expr<'ast>>,
	) -> Handled {
		let method_decl = unwrap_or_return!(cls.find_static_method(method_name), {
			self.add_diagnostic(loc, Diag::StaticMethodNotFound(cls.clone_ptr(), method_name));
			bogus(loc)
		});

		let inst_method = unwrap_or_return!(
			self.ctx
				.instantiate_method(&MethodOrAbstract::Method(method_decl.ptr()), ty_arg_asts),
			bogus(loc)
		);

		// No need to check selfEffect, because this is a static method.
		// Static methods can't look at their class' type arguments
		let args = unwrap_or_return!(
			self.check_call_arguments(loc, &inst_method, &Instantiator::nil(), arg_asts.iter()),
			bogus(loc)
		);

		let ty = instantiate_return_type(&inst_method);
		self.handle(&mut expected, loc, ExprData::StaticMethodCall(inst_method, args, ty))
	}

	//mv
	fn current_inst_cls(&self) -> InstCls {
		InstCls::generic_self_reference(self.ctx.current_class.ptr())
	}

	fn call_own_method<'ast>(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
		arg_asts: &'ast List<'ast, &'ast ast::Expr<'ast>>,
	) -> Handled {
		// Note: InstCls is still relevent here:
		// Even if 'self' is not an inst, in a superclass we will fill in type parameters.
		let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
			self.get_member_of_inst_cls(loc, &self.current_inst_cls(), method_name),
			bogus(loc)
		);

		//TODO: helper fn for converting member -> method
		let method_decl = match member_decl {
			MemberDeclaration::Method(m) => MethodOrAbstract::Method(m),
			MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(a),
			_ => {
				self.add_diagnostic(loc, Diag::DelegatesNotYetSupported);
				return bogus(loc)
			}
		};

		let inst_method =
			unwrap_or_return!(self.ctx.instantiate_method(&method_decl, ty_arg_asts), bogus(loc));

		let args = unwrap_or_return!(
			self.check_call_arguments(loc, &inst_method, &member_instantiator, arg_asts.iter()),
			bogus(loc)
		);

		let ty = instantiate_return_type(&inst_method);

		let expr = if method_decl.is_static() {
			// Calling own static method is OK.
			ExprData::StaticMethodCall(inst_method, args, ty)
		} else {
			if self.is_static {
				self.add_diagnostic(loc, Diag::CantCallInstanceMethodFromStaticMethod(method_decl));
				return bogus(loc)
			}

			if !self.self_effect.contains(method_decl.self_effect()) {
				self.add_diagnostic(
					loc,
					Diag::IllegalEffect { allowed: self.self_effect, required: method_decl.self_effect() },
				)
			}

			ExprData::MyInstanceMethodCall(inst_method, args, ty)
		};

		self.handle(&mut expected, loc, expr)
	}

	fn call_method<'ast, I : Iterator<Item=&'ast &'ast ast::Expr<'ast>>>(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		target_ast: &'ast ast::Expr<'ast>,
		method_name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
		arg_asts: I,
	) -> Handled {
		let target = self.check_infer(target_ast);
		let (inst_method, args, ty) = match *target.ty() {
			Ty::Bogus =>
				// Already issued an error, don't need another.
				return bogus(loc),
			Ty::Plain(target_effect, ref target_inst_cls) => {
				let InstMember(member_decl, member_instantiator) = unwrap_or_return!(
					self.get_member_of_inst_cls(loc, target_inst_cls, method_name),
					bogus(loc));

				let method = match member_decl {
					MemberDeclaration::Method(m) => MethodOrAbstract::Method(m),
					MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(a),
					_ => {
						self.add_diagnostic(loc, Diag::DelegatesNotYetSupported);
						return bogus(loc)
					}
				};

				if let MethodOrAbstract::Method(ref m) = method {
					if m.is_static {
						self.add_diagnostic(loc, Diag::CantAccessStaticMethodThroughInstance(m.clone_ptr()));
						return bogus(loc)
					}

					if !target_effect.contains(m.self_effect()) {
						self.add_diagnostic(loc, Diag::IllegalEffect { allowed: target_effect, required: m.self_effect() })
					}
				}

				// Note: member is instantiated based on the *class* type arguments,
                // but there may sill be *method* type arguments.
				let inst_method = unwrap_or_return!(self.ctx.instantiate_method(&method, ty_arg_asts), bogus(loc));

				let args = unwrap_or_return!(
					self.check_call_arguments(loc, &inst_method, &member_instantiator, arg_asts.into_iter()),
					bogus(loc));

				let ty = instantiate_return_type_with_extra_instantiator(&inst_method, &member_instantiator);
				(inst_method, args, ty)
			}
			Ty::Param(_) =>
				unimplemented!()
		};
		let e = ExprData::InstanceMethodCall(Box::new(target), inst_method, args, ty);
		self.handle(&mut expected, loc, e)
	}

	fn check_call_arguments<'ast, I : Iterator<Item=&'ast &'ast ast::Expr<'ast>>>(
		&self,
		loc: Loc,
		inst_method: &InstMethod,
		extra_instantiator: &Instantiator,
		arg_asts: I,
	) -> Option<Arr<Expr>> {
		let instantiator = Instantiator::of_inst_method(inst_method).combine(extra_instantiator);
		self.check_arguments(loc, &inst_method.0, &instantiator, arg_asts)
	}

	fn check_arguments<'ast, I : Iterator<Item=&'ast &'ast ast::Expr<'ast>>>(
		&self,
		loc: Loc,
		method_decl: &MethodOrAbstract,
		instantiator: &Instantiator,
		arg_asts: I,
	) -> Option<Arr<Expr>> {
		let parameters = method_decl.parameters();
		match parameters.try_zip(arg_asts, |parameter, arg_ast| {
			self.check_subtype(instantiate_type(&parameter.ty, instantiator), arg_ast)
		}) {
			Ok(res) => Some(res),
			Err((_, n_args)) => {
				self.add_diagnostic(loc, Diag::ArgumentCountMismatch(method_decl.copy(), n_args));
				None
			}
		}
	}

	/*
	NOTE: Caller is responsible for checking that we can access this member's effect!
	If this returns None, we've already handled the error reporting, so just call handleBogus.
	*/
	fn get_member_of_inst_cls(&self, loc: Loc, inst_cls: &InstCls, member_name: Sym) -> Option<InstMember> {
		let res = try_get_member_of_inst_cls(inst_cls, member_name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(inst_cls.0.clone_ptr(), member_name))
		}
		res
	}

	//TODO:inline
	fn get_own_slot(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		slot: Ptr<SlotDeclaration>,
		instantiator: Instantiator,
	) -> Handled {
		if self.is_static {
			self.add_diagnostic(loc, Diag::CantAccessSlotFromStaticMethod(slot));
			return bogus(loc)
		}

		if slot.mutable && !self.self_effect.can_get() {
			self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(slot.clone_ptr()))
		}

		let slot_ty = instantiate_and_narrow_effects(
			self.self_effect,
			&slot.ty,
			&instantiator,
			loc,
			&mut self.ctx.borrow_diags(),
		);
		self.handle(&mut expected, loc, ExprData::GetMySlot(slot, slot_ty))
	}

	fn handle(&self, expected: &mut Expected, loc: Loc, e: ExprData) -> Handled {
		match *expected {
			Expected::Return(ref ty) | Expected::SubTypeOf(ref ty) => self.check_ty(ty, loc, e),
			Expected::Infer(ref mut inferred_ty) => {
				let new_inferred_ty = match *inferred_ty {
					Some(ref mut last_inferred_ty) => self.get_compatible_type(loc, last_inferred_ty, e.ty()),
					None => e.ty().clone(),
				};
				*inferred_ty = Some(new_inferred_ty);
				Handled(Expr(loc, e))
			}
		}
	}

	fn check_ty(&self, expected_ty: &Ty, loc: Loc, e: ExprData) -> Handled {
		if is_assignable(expected_ty, e.ty()) {
			Handled(Expr(loc, e))
		} else {
			self.add_diagnostic(
				loc,
				Diag::NotAssignable { expected: expected_ty.clone(), actual: e.ty().clone() },
			);
			Handled(Expr(loc, ExprData::BogusCast(expected_ty.clone(), Box::new(Expr(loc, e)))))
		}
	}

	fn get_compatible_type(&self, loc: Loc, a: &Ty, b: &Ty) -> Ty {
		common_type(a, b).unwrap_or_else(|| {
			self.add_diagnostic(loc, Diag::CantCombineTypes(a.clone(), b.clone()));
			Ty::Bogus
		})
	}
}

fn bogus(loc: Loc) -> Handled {
	Handled(Expr(loc, ExprData::Bogus))
}

//TODO:NEATER



struct Handled(Expr);

enum Expected {
	/* Return is identical to SubTypeOf, but marks that we're in a tail call position. */
	Return(Ty),
	SubTypeOf(Ty),
	/* Expected should always be passed by `&mut`, so that inferred types can be inserted here. */
	Infer(Option<Ty>), //todo: unsafecell?
}
impl Expected {
	fn inferred_ty(&self) -> &Ty {
		self.current_expected_ty().unwrap()
	}

	fn current_expected_ty(&self) -> Option<&Ty> {
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

//mv
fn instantiate_return_type(inst_method: &InstMethod) -> Ty {
	instantiate_type(inst_method.0.return_ty(), &Instantiator::of_inst_method(inst_method))
}

fn instantiate_return_type_with_extra_instantiator(
	inst_method: &InstMethod,
	instantiator: &Instantiator,
) -> Ty {
	instantiate_type(
		inst_method.0.return_ty(),
		&Instantiator::of_inst_method(inst_method).combine(instantiator),
	)
}
