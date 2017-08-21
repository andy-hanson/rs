use std::cell::RefCell;

use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{ Own, Ptr };
use util::sym::Sym;

use super::super::diag::{ DiagnosticData };
use super::super::model::class::{ ClassDeclaration, ClassHead, MemberDeclaration, SlotDeclaration };
use super::super::model::effect::{ Effect, EFFECT_MAX };
use super::super::model::expr::{ Case, Catch, Expr, ExprData, Local, Pattern };
use super::super::model::method::{ MethodOrAbstract, MethodInst, MethodOrImpl, Parameter };
use super::super::model::ty::{ InstCls, Ty };
use super::super::parse::ast;

use super::ctx::Ctx;
use super::class_utils::{ try_get_member_of_inst_cls, InstMember };
use super::ty_replacer::TyReplacer;
use super::ty_utils::{
	get_common_compatible_type, instantiate_ty, instantiate_ty_and_narrow_effects, is_assignable
};

pub fn check_method_body(
	ctx: &Ctx,
	method_or_impl: &MethodOrImpl,
	method_replacer: &TyReplacer,
	is_static: bool,
	body: &ast::Expr) -> Expr {
	unused!(ctx, method_replacer, is_static, body);
	let signature = method_or_impl.signature();
	let return_ty = instantiate_ty(&signature.return_ty, method_replacer);
	let ectx = Ectx {
		ctx,
		method_or_impl,
		method_replacer,
		is_static,
		self_effect: signature.self_effect,
		current_parameters: &signature.parameters,
		locals: RefCell::new(Vec::new())
	};
	ectx.check_return(return_ty, body)
}

struct Ectx<'a> {
	ctx: &'a Ctx,
	method_or_impl: &'a MethodOrImpl,
	method_replacer: &'a TyReplacer,
	is_static: bool,
	self_effect: Effect,
	current_parameters: &'a Arr<Own<Parameter>>,
	locals: RefCell<Vec<Ptr<Local>>>,
}
impl<'a> Ectx<'a> {
	fn add_diagnostic(&self, loc: Loc, data: DiagnosticData) {
		self.ctx.add_diagnostic(loc, data)
	}

	fn check_return(&self, ty: Ty, a: &ast::Expr) -> Expr {
		self.check_expr(&mut Expected::Return(ty), a)
	}

	fn check_subtype(&self, ty: Ty, a: &ast::Expr) -> Expr {
		self.check_expr(&mut Expected::SubTypeOf(ty), a)
	}

	fn check_void(&self, a: &ast::Expr) -> Expr {
		// Need to access builtins::void
		unused!(a);
		todo!()
	}

	fn check_bool(&self, a: &ast::Expr) -> Expr {
		unused!(a);
		todo!()
	}

	fn check_infer(&self, a: &ast::Expr) -> Expr {
		self.check_expr(&mut Expected::Infer(None), a)
	}

	fn check_expr(&self, mut e: &mut Expected, a: &ast::Expr) -> Expr {
		self.check_expr_worker(&mut e, &a).0
	}

	fn check_expr_worker(&self, mut e: &mut Expected, &ast::Expr(loc, ref ast_data): &ast::Expr) -> Handled {
		match ast_data {
			&ast::ExprData::Access(name) => {
				if let Some(local) = self.locals.borrow().iter().find(|l| l.name == name) {
					return self.handle(&mut e, loc, ExprData::AccessLocal(local.clone_ptr()))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = instantiate_ty(&param.ty, self.method_replacer);
					return self.handle(&mut e, loc, ExprData::AccessParameter(param.ptr(), ty))
				}

				if let Some(InstMember(member_decl, member_ty_replacer)) = self.ctx.get_own_member_or_add_diagnostic(loc, name) {
					match member_decl {
						MemberDeclaration::Slot(slot) =>
							self.get_own_slot(&mut e, loc, slot, member_ty_replacer),
						MemberDeclaration::Method(_) | MemberDeclaration::AbstractMethod(_) =>
							todo!(), //diagnostic
					}
				} else {
					handle_bogus(&mut e, loc)
				}
			}
			&ast::ExprData::StaticAccess(_, _) | &ast::ExprData::TypeArguments(_, _) => {
				self.add_diagnostic(loc, DiagnosticData::DelegatesNotYetSupported);
				handle_bogus(&mut e, loc)
			}
			&ast::ExprData::OperatorCall(ref left, operator, ref right) => {
				let ty_args = Arr::<ast::Ty>::empty(); // No way to provide these to an operator call.
				self.call_method(&mut e, loc, &left, operator, &ty_args, ArgAsts::One(right.as_ref()))
			}
			&ast::ExprData::Call(ref target, ref args) =>
				self.check_call_ast_worker(&mut e, loc, target, args),
			&ast::ExprData::Recur(ref arg_asts) => {
				if !e.in_tail_call_position() {
					self.add_diagnostic(loc, DiagnosticData::NotATailCall)
				}
				// For recursion, need to do substitution in the case that we are implementing an abstract method where the superclass took type arguments.
				let args = match self.do_check_call_arguments(loc, &self.method_or_impl.method_or_abstract(), self.method_replacer, ArgAsts::Many(arg_asts)) {
					Some(a) => a,
					None => return handle_bogus(&mut e, loc)
				};
				self.handle(&mut e, loc, ExprData::Recur(self.method_or_impl.clone(), args))
			}
			&ast::ExprData::New(ref ty_arg_asts, ref arg_asts) => {
				let slots = match self.ctx.current_class.head() {
					&ClassHead::Slots(_, ref slots) => slots,
					_ => {
						self.add_diagnostic(loc, DiagnosticData::NewInvalid(self.ctx.current_class.ptr()));
						return handle_bogus(&mut e, loc)
					}
				};
				if arg_asts.len() != slots.len() {
					self.add_diagnostic(loc, DiagnosticData::NewArgumentCountMismatch(slots.len(), arg_asts.len()));
					return handle_bogus(&mut e, loc)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len() {
					todo!()
				}

				let ty_args = self.get_ty_args(ty_arg_asts);
				let inst_cls = InstCls(self.ctx.current_class.ptr(), ty_args);
				let replacer = TyReplacer::of_inst_cls(&inst_cls);
				let args = arg_asts.zip(&slots, |arg, slot|
					self.check_subtype(instantiate_ty(&slot.ty, &replacer), arg));
				let ty = Ty::Plain(EFFECT_MAX, inst_cls);
				self.handle(&mut e, loc, ExprData::New(ty, args))
			}
			&ast::ExprData::ArrayLiteral(ref element_ty, ref args) => {
				unused!(element_ty, args);
				todo!()
			}
			&ast::ExprData::GetProperty(ref target_ast, property_name) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match target.ty() {
					&Ty::Bogus =>
						return handle_bogus(&mut e, loc),
					&Ty::Plain(target_effect, ref target_class) => {
						let InstMember(member_decl, replacer) =
							//TODO: just get_slot_of_inst_cls_or_add_diagnostic
							match self.get_member_of_inst_cls_or_add_diagnostic(target.loc(), &target_class, property_name) {
								Some(m) => m,
								None => return handle_bogus(&mut e, loc),
							};
						let slot = match member_decl {
							MemberDeclaration::Slot(s) => s,
							_ => {
								self.add_diagnostic(target.loc(), DiagnosticData::DelegatesNotYetSupported);
								return handle_bogus(&mut e, loc)
							}
						};
						if slot.mutable && !target_effect.can_get() {
							self.add_diagnostic(loc, DiagnosticData::MissingEffectToGetSlot(slot.clone_ptr()))
						}
						let slot_ty = instantiate_ty_and_narrow_effects(target_effect, &slot.ty, &replacer, loc, &mut self.ctx.borrow_diags());
						(slot, slot_ty)
					},
					&Ty::Param(_) =>
						todo!()
				};
				self.handle(&mut e, loc, ExprData::GetSlot(Box::new(target), slot, slot_ty))
			}
			&ast::ExprData::SetProperty(property_name, ref value_ast) => {
				let InstMember(member_decl, replacer) = match self.ctx.get_own_member_or_add_diagnostic(loc, property_name) {
					Some(m) => m,
					None => return handle_bogus(&mut e, loc)
				};
				let slot = match member_decl {
					MemberDeclaration::Slot(s) => s,
					_ => {
						self.add_diagnostic(loc, DiagnosticData::CantSetNonSlot(member_decl));
						return handle_bogus(&mut e, loc)
					}
				};
				if !slot.mutable {
					self.add_diagnostic(loc, DiagnosticData::SlotNotMutable(slot.clone_ptr()))
				}
				if !self.self_effect.can_set() {
					self.add_diagnostic(loc, DiagnosticData::MissingEffectToSetSlot(self.self_effect, slot.clone_ptr()))
				}

				let value = self.check_subtype(instantiate_ty(&slot.ty, &replacer), value_ast);
				self.handle(&mut e, loc, ExprData::SetSlot(slot, Box::new(value)))
			}
			&ast::ExprData::LetInProgress(_, _) =>
				unreachable!(),
			&ast::ExprData::Let(ref pattern_ast, ref value_ast, ref then_ast) => {
				let value = self.check_infer(value_ast);
				let (pattern, n_added) = {//self.start_check_pattern(value.ty(), pattern_ast);
					let &ast::Pattern(pattern_loc, ref pattern_data) = pattern_ast;
					match pattern_data {
						&ast::PatternData::Ignore =>
							(Pattern::Ignore, 0),
						&ast::PatternData::Single(name) => {
							let local = Own::new(Local { loc: pattern_loc, ty: value.ty().clone(), name });
							self.add_to_scope(local.ptr());
							(Pattern::Single(local), 1)
						}
						&ast::PatternData::Destruct(_) => {
							todo!()
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
			&ast::ExprData::Seq(ref first_ast, ref then_ast) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(&mut e, then_ast);
				// 'expected' was handled in 'then'
				Handled(Expr(loc, ExprData::Seq(Box::new(first), Box::new(then))))
			}
			&ast::ExprData::Literal(ref value) => {
				unused!(value);
				todo!()
			}
			&ast::ExprData::SelfExpr => {
				/*
				Create an InstCls for the current class that just maps type parameters to theirselves.
				For example:
					class Foo[T]
						Foo[T] get-self()
							self || This is of type Foo[T] where T is the same as the type parameter on Foo.

					fun use-it(Foo[Int] foo)
						foo.get-self() || The return type has the same T, so it will be instantiated to return Foo[Int].
				*/
				self.handle(&mut e, loc, ExprData::SelfExpr(Ty::Plain(self.self_effect, self.current_inst_cls())))
			}
			&ast::ExprData::IfElse(ref condition_ast, ref then_ast, ref else_ast) => {
				let condition = self.check_bool(condition_ast);
				let then = self.check_expr(&mut e, then_ast);
				let elze = self.check_expr(&mut e, else_ast);
				// 'expected' was handled in both 'then' and 'else'.
				Handled(Expr(loc, ExprData::IfElse(Box::new(condition), Box::new(then), Box::new(elze), e.inferred_ty().clone())))
			}
			&ast::ExprData::WhenTest(ref case_asts, ref else_ast) => {
				let cases = case_asts.map(|&ast::Case(case_loc, ref test_ast, ref result_ast)| {
					let test = self.check_bool(&test_ast);
					let result = self.check_expr(&mut e, &result_ast);
					Case(case_loc, Box::new(test), Box::new(result))
				});
				let elze = self.check_expr(&mut e, else_ast);
				// 'expected' handled in each case result and in 'elze'.
				Handled(Expr(loc, ExprData::WhenTest(cases, Box::new(elze), e.inferred_ty().clone())))
			}
			&ast::ExprData::Assert(ref asserted_ast) => {
				let asserted = self.check_bool(asserted_ast);
				self.handle(&mut e, loc, ExprData::Assert(Box::new(asserted)))
			}
			&ast::ExprData::Try(ref do_ast, ref op_catch_ast, ref op_finally_ast) => {
				let do_ = self.check_expr(&mut e, do_ast);
				/*
				pub loc: Loc,
				pub exception_ty: Ty,
				pub exception_name_loc: Loc,
				pub exception_name: Sym,
				pub then: Box<Expr>,
				*/
				let catch = op_catch_ast.as_ref().map(|&ast::Catch { loc: catch_loc, exception_type: ref exception_type_ast, exception_name_loc, exception_name, then: ref then_ast }| {
					let exception_ty = self.ctx.get_ty(exception_type_ast);
					let caught = Own::new(Local { loc: exception_name_loc, ty: exception_ty, name: exception_name });
					self.add_to_scope(caught.ptr());
					let then = self.check_expr(&mut e, then_ast);
					self.pop_from_scope();
					Catch(catch_loc, caught, Box::new(then))
				});
				let finally = op_finally_ast.as_ref().map(|f| Box::new(self.check_void(&f)));
				// 'expected' handled when checking 'do' and 'catch'
				Handled(Expr(loc, ExprData::Try(Box::new(do_), catch, finally, e.inferred_ty().clone())))
			}
			&ast::ExprData::For(_, _, _) => //(local_name, ref looper, ref body) =>
				todo!(),
		}
	}

	//mv
	fn add_to_scope(&self, local: Ptr<Local>) {
		// It's important that we push even in the presence of errors, because we will always pop.
		if let Some(param) = self.current_parameters.find(|p| p.name == local.name) {
			self.add_diagnostic(local.loc, DiagnosticData::CantReassignParameter(param.ptr()))
		}
		if let Some(old_local) = self.locals.borrow().iter().find(|l| l.name == local.name) {
			self.add_diagnostic(local.loc, DiagnosticData::CantReassignLocal(old_local.clone_ptr()))
		}
		self.locals.borrow_mut().push(local)
	}
	fn pop_from_scope(&self) {
		let popped = self.locals.borrow_mut().pop();
		assert!(popped.is_some())
	}

	fn check_call_ast_worker(&self, mut expected: &mut Expected, loc: Loc, target: &ast::Expr, args: &Arr<ast::Expr>) -> Handled{
		let &ast::Expr(target_loc, ref target_data) = target;
		let arr_empty = Arr::empty();
		let (&ast::Expr(real_target_loc, ref real_target_data), ty_arg_asts) = match target_data {
			&ast::ExprData::TypeArguments(ref real_target, ref ty_arg_asts) => (real_target.as_ref(), ty_arg_asts),
			_ => (target, &arr_empty),
		};
		match real_target_data {
			&ast::ExprData::StaticAccess(class_name, static_method_name) =>
				match self.ctx.access_class_declaration_or_add_diagnostic(real_target_loc, class_name) {
					Some(cls) => self.call_static_method(&mut expected, loc, cls, static_method_name, ty_arg_asts, args),
					None => handle_bogus(&mut expected, target_loc)
				},
			&ast::ExprData::GetProperty(ref property_target, property_name) =>
				self.call_method(&mut expected, loc, property_target, property_name, ty_arg_asts, ArgAsts::Many(args)),
			&ast::ExprData::Access(name) =>
				self.call_own_method(&mut expected, loc, name, ty_arg_asts, args),
			_ => {
				self.add_diagnostic(loc, DiagnosticData::DelegatesNotYetSupported);
				handle_bogus(&mut expected, target_loc)
			}
		}
	}

	fn call_static_method(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		cls: Ptr<ClassDeclaration>,
		method_name: Sym,
		ty_arg_asts: &Arr<ast::Ty>,
		arg_asts: &Arr<ast::Expr>) -> Handled {
		let method_decl = match cls.find_static_method(method_name) {
			Some(m) => m,
			None => {
				self.add_diagnostic(loc, DiagnosticData::StaticMethodNotFound(cls.clone_ptr(), method_name));
				return handle_bogus(&mut expected, loc)
			}
		};

		let method_inst = match self.instantiate_method_or_add_diagnostic(&MethodOrAbstract::Method(method_decl.ptr()), ty_arg_asts) {
			Some(m) => m,
			None => return handle_bogus(&mut expected, loc),
		};

		// No need to check selfEffect, because this is a static method.
		// Static methods can't look at their class' type arguments
		let args = match self.check_call_arguments(loc, &method_inst, &TyReplacer::do_nothing(), ArgAsts::Many(arg_asts)) {
			Some(a) => a,
			None => return handle_bogus(&mut expected, loc)
		};

		let ty = instantiate_return_type(&method_inst);
		self.handle(&mut expected, loc, ExprData::StaticMethodCall(method_inst, args, ty))
	}

	//mv
	fn current_inst_cls(&self) -> InstCls {
		InstCls::generic_self_reference(self.ctx.current_class.ptr())
	}

	fn call_own_method(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: &Arr<ast::Ty>,
		arg_asts: &Arr<ast::Expr>) -> Handled {
		/*
		Note: InstCls is still relevent here; even if 'self' is not an inst, in a superclass we will fill in type parameters.
		*/
		let InstMember(member_decl, member_replacer) =
			match self.get_member_of_inst_cls_or_add_diagnostic(loc, &self.current_inst_cls(), method_name) {
				Some(m) => m,
				None => return handle_bogus(&mut expected, loc)
			};

		//TODO: helper fn for converting member -> method
		let method_decl = match member_decl {
			MemberDeclaration::Method(m) => MethodOrAbstract::Method(m),
			MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(a),
			_ => {
				self.add_diagnostic(loc, DiagnosticData::DelegatesNotYetSupported);
				return handle_bogus(&mut expected, loc)
			}
		};

		let method_inst = match self.instantiate_method_or_add_diagnostic(&method_decl, ty_arg_asts) {
			Some(m) => m,
			None => return handle_bogus(&mut expected, loc),
		};

		let args = match self.check_call_arguments(loc, &method_inst, &member_replacer, ArgAsts::Many(arg_asts)) {
			Some(a) => a,
			None => return handle_bogus(&mut expected, loc),
		};

		let ty = instantiate_return_type(&method_inst);

		let expr = if method_decl.is_static() {
			// Calling own static method is OK.
			ExprData::StaticMethodCall(method_inst, args, ty)
		} else {
			if self.is_static {
				self.add_diagnostic(loc, DiagnosticData::CantCallInstanceMethodFromStaticMethod(method_decl));
				return handle_bogus(&mut expected, loc)
			}

			if !self.self_effect.contains(method_decl.self_effect()) {
				self.add_diagnostic(loc, DiagnosticData::IllegalEffect(self.self_effect, method_decl.self_effect()))
			}

			ExprData::MyInstanceMethodCall(method_inst, args, ty)
		};

		self.handle(&mut expected, loc, expr)
	}

	fn call_method(
		&self,
		mut expected: &mut Expected,
		loc: Loc,
		target_ast: &ast::Expr,
		method_name: Sym,
		ty_arg_asts: &Arr<ast::Ty>,
		arg_asts: ArgAsts) -> Handled {
		let target = self.check_infer(target_ast);
		let (method_inst, args, ty) = match target.ty() {
			&Ty::Bogus =>
				// Already issued an error, don't need another.
				return handle_bogus(&mut expected, loc),
			&Ty::Plain(target_effect, ref target_inst_cls) => {
				let InstMember(member_decl, member_replacer) = match self.get_member_of_inst_cls_or_add_diagnostic(loc, &target_inst_cls, method_name) {
					Some(member) => member,
					None => return handle_bogus(&mut expected, loc)
				};

				let method = match member_decl {
					MemberDeclaration::Method(m) => MethodOrAbstract::Method(m),
					MemberDeclaration::AbstractMethod(a) => MethodOrAbstract::Abstract(a),
					_ => {
						self.add_diagnostic(loc, DiagnosticData::DelegatesNotYetSupported);
						return handle_bogus(&mut expected, loc)
					}
				};

				if let MethodOrAbstract::Method(ref m) = method {
					if m.is_static {
						self.add_diagnostic(loc, DiagnosticData::CantAccessStaticMethodThroughInstance(m.clone_ptr()));
						return handle_bogus(&mut expected, loc)
					}

					if !target_effect.contains(m.self_effect()) {
						self.add_diagnostic(loc, DiagnosticData::IllegalEffect(target_effect, m.self_effect()))
					}
				}

				// Note: member is instantiated based on the *class* type arguments, but there may sill be *method* type arguments.
				let method_inst = match self.instantiate_method_or_add_diagnostic(&method, &ty_arg_asts) {
					Some(i) => i,
					None => return handle_bogus(&mut expected, loc)
				};

				let args = match self.check_call_arguments(loc, &method_inst, &member_replacer, arg_asts) {
					Some(a) => a,
					None => return handle_bogus(&mut expected, loc)
				};

				let ty = instantiate_return_type_with_extra_replacer(&method_inst, &member_replacer);
				(method_inst, args, ty)
			}
			&Ty::Param(_) =>
				todo!()
		};
		let e = ExprData::InstanceMethodCall(Box::new(target), method_inst, args, ty);
		self.handle(&mut expected, loc, e)
	}

	fn check_call_arguments(
		&self,
		loc: Loc,
		method_inst: &MethodInst,
		replacer: &TyReplacer,
		arg_asts: ArgAsts)
		-> Option<Arr<Expr>> {

		let method_decl = &method_inst.0;
		let replacer = TyReplacer::of_inst_method(&method_inst).combine(replacer);
		self.do_check_call_arguments(loc, &method_decl, &replacer, arg_asts)
	}

	fn do_check_call_arguments(
		&self,
		loc: Loc,
		method_decl: &MethodOrAbstract,
		replacer: &TyReplacer,
		arg_asts: ArgAsts)
		-> Option<Arr<Expr>> {
		let parameters = method_decl.parameters();
		match arg_asts {
			ArgAsts::One(arg_ast) => {
				if let Some(parameter) = parameters.only() {
					Some(Arr::_1(self.check_subtype(instantiate_ty(&parameter.ty, &replacer), arg_ast)))
				} else {
					self.add_diagnostic(loc, DiagnosticData::ArgumentCountMismatch(method_decl.clone(), 1));
					None
				}
			}
			ArgAsts::Many(arg_asts_array) => {
				if parameters.len() != arg_asts_array.len() {
					self.add_diagnostic(loc, DiagnosticData::ArgumentCountMismatch(method_decl.clone(), arg_asts_array.len()));
					None
				} else {
					Some(parameters.zip(arg_asts_array, |parameter, arg_ast| {
						self.check_subtype(instantiate_ty(&parameter.ty, &replacer), arg_ast)
					}))
				}
			}
		}

	}

	//mv
	fn get_ty_args(&self, ty_arg_asts: &Arr<ast::Ty>) -> Arr<Ty> {
		ty_arg_asts.map(|ty_ast| self.ctx.get_ty(ty_ast))
	}

	fn instantiate_method_or_add_diagnostic(&self, method_decl: &MethodOrAbstract, ty_arg_asts: &Arr<ast::Ty>) -> Option<MethodInst> {
		if ty_arg_asts.len() != method_decl.type_parameters().len() {
			todo!()
		}
		Some(MethodInst(method_decl.clone(), self.get_ty_args(ty_arg_asts)))
	}

	/**
	NOTE: Caller is responsible for checking that we can access this member's effect!
	If this returns None, we've already handled the error reporting, so just call handleBogus.
	*/
	fn get_member_of_inst_cls_or_add_diagnostic(&self, loc: Loc, inst_cls: &InstCls, member_name: Sym) -> Option<InstMember> {
		let res = try_get_member_of_inst_cls(&inst_cls, member_name);
		if res.is_none() {
			self.add_diagnostic(loc, DiagnosticData::MemberNotFound(inst_cls.0.clone_ptr(), member_name))
		}
		res
	}

	//TODO:inline
	fn get_own_slot(&self, mut expected: &mut Expected, loc: Loc, slot: Ptr<SlotDeclaration>, replacer: TyReplacer) -> Handled {
		if self.is_static {
			self.add_diagnostic(loc, DiagnosticData::CantAccessSlotFromStaticMethod(slot));
			return handle_bogus(&mut expected, loc)
		}

		if slot.mutable && !self.self_effect.can_get() {
			self.add_diagnostic(loc, DiagnosticData::MissingEffectToGetSlot(slot.clone_ptr()))
		}

		let slot_ty = instantiate_ty_and_narrow_effects(self.self_effect, &slot.ty, &replacer, loc, &mut self.ctx.borrow_diags());
		self.handle(&mut expected, loc, ExprData::GetMySlot(slot, slot_ty))
	}

	fn handle(&self, mut expected: &mut Expected, loc: Loc, e: ExprData) -> Handled {
		match expected {
			&mut Expected::Return(ref ty) | &mut Expected::SubTypeOf(ref ty) =>
				self.check_ty(ty, loc, e),
			&mut Expected::Infer(ref mut inferred_ty) => {
				let new_inferred_ty = match inferred_ty {
					&mut Some(ref mut last_inferred_ty) =>
						self.get_compatible_type(loc, last_inferred_ty, e.ty()),
					&mut None =>
						e.ty().clone(),
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
			self.add_diagnostic(loc, DiagnosticData::NotAssignable(expected_ty.clone(), e.ty().clone()));
			Handled(Expr(loc, ExprData::BogusCast(expected_ty.clone(), Box::new(Expr(loc, e)))))
		}
	}

	fn get_compatible_type(&self, loc: Loc, a: &Ty, b: &Ty) -> Ty {
		match get_common_compatible_type(a, b) {
			Some(c) => c,
			None => {
				self.add_diagnostic(loc, DiagnosticData::CantCombineTypes(a.clone(), b.clone()));
				Ty::Bogus
			}
		}
	}
}

fn handle_bogus(expected: &mut Expected, loc: Loc) -> Handled {
	let ty = match expected.current_expected_ty() { Some(t) => t.clone(), None => Ty::Bogus };
	Handled(Expr(loc, ExprData::Bogus(ty)))
}

struct Handled(Expr);

enum Expected {
	/** Return is identical to SubTypeOf, but marks that we're in a tail call position. */
	Return(Ty),
	SubTypeOf(Ty),
	/** Expected should always be passed by `&mut`, so that inferred types can be inserted here. */
	Infer(Option<Ty>), //todo: unsafecell?
}
impl Expected {
	fn inferred_ty(&self) -> &Ty {
		self.current_expected_ty().unwrap()
	}

	fn current_expected_ty(&self) -> Option<&Ty> {
		match self {
			&Expected::Return(ref ty) | &Expected::SubTypeOf(ref ty) =>
				Some(ty),
			&Expected::Infer(ref ty_op) =>
				ty_op.as_ref(),
		}
	}

	fn in_tail_call_position(&self) -> bool {
		match self {
			&Expected::Return(_) => true,
			_ => false
		}
	}
}

// Needed to make check_call work in both cases
// TODO: use a trait, and make fns on this generic
enum ArgAsts<'a> {
	One(&'a ast::Expr),
	Many(&'a Arr<ast::Expr>)
}

//mv
fn instantiate_return_type(inst_method: &MethodInst) -> Ty {
	instantiate_ty(inst_method.0.return_ty(), &TyReplacer::of_inst_method(inst_method))
}

fn instantiate_return_type_with_extra_replacer(inst_method: &MethodInst, replacer: &TyReplacer) -> Ty {
	instantiate_ty(inst_method.0.return_ty(), &TyReplacer::of_inst_method(inst_method).combine(replacer))
}

