use std::cell::UnsafeCell;
use std::ops::Try;

use util::arena::Arena;
use util::iter::{KnownLen, OptionIter};
use util::late::Late;
use util::list::List;
use util::loc::Loc;
use util::sym::Sym;
use util::up::Up;

use ast;

use model::class::{ClassDeclaration, ClassHead, SlotDeclaration};
use model::diag::Diag;
use model::effect::Effect;
use model::expr::{Case, Catch, Expr, ExprData, GetSlotData, IfElseData, InstanceMethodCallData, LetData,
                  Local, MyInstanceMethodCallData, Pattern, SeqData, SetSlotData, StaticMethodCallData,
                  TryData, WhenTestData};
use model::method::{InstMethod, MethodOrImpl, MethodOrImplOrAbstract, Parameter};
use model::ty::{PlainTy, Ty};

use super::class_utils::MethodAndInferrer;
use super::ctx::Ctx;
use super::expected::Expected;
use super::inferrer::Inferrer;
use super::instantiator::Instantiator;
use super::ty_utils::{check_assignable, instantiate_and_narrow_effects, instantiate_ty};

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
) -> Expr<'model> {
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

	fn arena(&self) -> &'model Arena {
		self.ctx.arena
	}

	fn check_return<'ast>(&mut self, ty: &Ty<'model>, a: &'ast ast::Expr<'ast>) -> Expr<'model> {
		self.check_return_or_subtype(ty, a, true)
	}

	//TODO: this is always called immediately after instantiate_ty, combine?
	fn check_subtype<'ast>(&mut self, ty: &Ty<'model>, a: &'ast ast::Expr<'ast>) -> Expr<'model> {
		self.check_return_or_subtype(ty, a, false)
	}

	fn check_return_or_subtype<'ast>(
		&mut self,
		ty: &Ty<'model>,
		a: &'ast ast::Expr<'ast>,
		in_tail_call_position: bool,
	) -> Expr<'model> {
		let expected_ty = UnsafeCell::new(Some(ty.clone()));
		let expected =
			Expected { in_tail_call_position, expected_ty: &expected_ty, inferrer: &Inferrer::nil() };
		self.check_expr(expected, a)
	}

	fn check_void<'ast>(&mut self, ast: &'ast ast::Expr<'ast>) -> Expr<'model> {
		self.check_subtype(&*self.ctx.builtins.void, ast)
	}

	fn check_bool<'ast>(&mut self, ast: &'ast ast::Expr<'ast>) -> Expr<'model> {
		self.check_subtype(&*self.ctx.builtins.bool, ast)
	}

	fn check_infer<'ast>(&mut self, ast: &'ast ast::Expr<'ast>) -> Expr<'model> {
		let inferred = UnsafeCell::new(None);
		self.check_expr(
			Expected { in_tail_call_position: false, expected_ty: &inferred, inferrer: &Inferrer::nil() },
			ast,
		)
	}

	fn check_expr<'ast, 'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		expr_ast: &'ast ast::Expr<'ast>,
	) -> Expr<'model> {
		self.check_expr_worker(expected, expr_ast)
			.0
			.unwrap_or_else(|| Expr { loc: expr_ast.loc, ty: Ty::Bogus, data: ExprData::Bogus })
	}

	fn check_expr_worker<'ast, 'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		&ast::Expr { loc, data: ref ast_data }: &'ast ast::Expr<'ast>,
	) -> Handled<'model> {
		match *ast_data {
			ast::ExprData::Access(name) => {
				if let Some(local) = self.try_find_local(name) {
					return self.handle(expected, loc, local.ty.clone(), ExprData::AccessLocal(Up(local)))
				}

				if let Some(param) = self.current_parameters.iter().find(|p| p.name == name) {
					let ty = self.instantiate_ty(&param.ty, self.method_instantiator);
					return self.handle(expected, loc, ty, ExprData::AccessParameter(Up(param)))
				}

				let slot = O(self.ctx.get_own_slot(loc, name))?;
				self.get_my_slot(expected, loc, slot)
			}
			ast::ExprData::StaticAccess { .. } | ast::ExprData::TypeArguments(_) => {
				self.add_diagnostic(loc, Diag::MethodUsedAsValue);
				Handled(None)
			}
			ast::ExprData::OperatorCall(&ast::OperatorCallData { ref left, operator, ref right }) => {
				let ty_args = List::EMPTY; // No way to provide these to an operator call.
				self.call_instance_method(expected, loc, left, operator, ty_args, OptionIter(Some(right)))
			}
			ast::ExprData::Call(&ast::CallData { ref target, args }) => {
				let &ast::Expr { loc: _, data: ref target_data } = target;
				let (&ast::Expr { loc: real_target_loc, data: ref real_target_data }, ty_arg_asts) =
					match *target_data {
						ast::ExprData::TypeArguments(
							&ast::TypeArgumentsData { target: ref real_target, type_arguments: ty_arg_asts },
						) => (real_target, ty_arg_asts),
						_ => (target, List::EMPTY),
					};
				match *real_target_data {
					ast::ExprData::StaticAccess { class_name, static_method_name } => {
						let class = O(
							self.ctx
								.access_class_declaration_or_add_diagnostic(real_target_loc, class_name),
						)?;
						self.call_static_method(expected, loc, class, static_method_name, ty_arg_asts, args)
					}
					ast::ExprData::GetProperty(&ast::GetPropertyData(ref property_target, property_name)) =>
						self.call_instance_method(
							expected,
							loc,
							property_target,
							property_name,
							ty_arg_asts,
							args,
						),
					ast::ExprData::Access(name) =>
						self.call_my_method(expected, loc, name, ty_arg_asts, args),
					_ => {
						self.add_diagnostic(loc, Diag::CallsNonMethod);
						Handled(None)
					}
				}
			}
			ast::ExprData::Recur(arg_asts) => {
				if !expected.in_tail_call_position {
					self.add_diagnostic(loc, Diag::NotATailCall)
				}
				// For recursion, need to do substitution in case we are
	// implementing an abstract method where the superclass took type arguments.
				let method_or_impl = self.method_or_impl;
				let moa = method_or_impl.method_or_abstract();
				let inst = self.method_instantiator;
				// We don't need to perform type inference here, because we're going back to the top of the same method.
				let _ = arg_asts;
				let _ = moa;
				let _ = inst;
				unimplemented!()
				//let args = //O(self.check_arguments(loc, moa, inst, arg_asts))?;
	//let data = self.arena() <- RecurData(method_or_impl, args);
	//self.handle(expected, loc, method_or_impl.return_ty().clone(), ExprData::Recur(data))
			}
			ast::ExprData::New(&ast::NewData(ty_arg_asts, arg_asts)) => {
				let slots = match *self.ctx.current_class.up_ref().head {
					ClassHead::Slots(ref data) => &*data.slots,
					_ => {
						let diag = Diag::NewInvalid(self.ctx.current_class);
						self.add_diagnostic(loc, diag);
						return Handled(None)
					}
				};
				if arg_asts.len() != slots.len() {
					let diag = Diag::NewArgumentCountMismatch {
						class: self.ctx.current_class,
						n_slots: slots.len(),
						n_arguments: arg_asts.len(),
					};
					self.add_diagnostic(loc, diag);
					return Handled(None)
				}

				if self.ctx.current_class.type_parameters.len() != ty_arg_asts.len() {
					unimplemented!()
				}

				let inst_class = O({
					let current_class = self.ctx.current_class;
					self.ctx.instantiate_class(current_class, ty_arg_asts)
				})?;
				let instantiator = Instantiator::of_inst_class(&inst_class);
				let args = self.ctx.arena.map(slots.zip(arg_asts), |(slot, arg)| {
					let ty = self.instantiate_ty(&slot.ty, &instantiator);
					self.check_subtype(&ty, arg)
				});
				let ty = Ty::io(inst_class);
				self.handle(expected, loc, ty, ExprData::New(args))
			}
			ast::ExprData::ArrayLiteral(&ast::ArrayLiteralData(ref _element_ty, _args)) => unimplemented!(),
			ast::ExprData::GetProperty(&ast::GetPropertyData(ref target_ast, property_name)) => {
				let target = self.check_infer(target_ast);
				let (slot, slot_ty) = match target.ty {
					Ty::Bogus => return Handled(None),
					Ty::Plain(PlainTy { effect: target_effect, ref inst_class }) => {
						let slot = O(self.ctx.get_slot(loc, inst_class.class, property_name))?;
						if slot.mutable && !target_effect.can_get() {
							self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(slot))
						}
						let slot_ty = instantiate_and_narrow_effects(
							target_effect,
							&slot.ty,
							&Instantiator::of_inst_class(inst_class),
							loc,
							&mut self.ctx.diags,
							self.ctx.arena,
						);
						(slot, slot_ty)
					}
					Ty::Param(_) => unimplemented!(),
				};
				let data = self.arena() <- GetSlotData { target, slot };
				self.handle(expected, loc, slot_ty, ExprData::GetSlot(data))
			}
			ast::ExprData::SetProperty(&ast::SetPropertyData(property_name, ref value_ast)) => {
				let slot = O(self.ctx.get_own_slot(loc, property_name))?;
				if !slot.mutable {
					self.add_diagnostic(loc, Diag::SlotNotMutable(slot))
				}
				let allowed_effect = self.self_effect;
				if !allowed_effect.can_set() {
					self.add_diagnostic(loc, Diag::MissingEffectToSetSlot { allowed_effect, slot })
				}

				// No need to instantiate slot.ty
				let value = self.check_subtype(&slot.ty, value_ast);
				let data = self.arena() <- SetSlotData(slot, value);
				self.handle(expected, loc, self.ctx.builtins.void.clone(), ExprData::SetSlot(data))
			}
			ast::ExprData::Let(&ast::LetData(ref pattern_ast, ref value_ast, ref then_ast)) => {
				let value = self.check_infer(value_ast);
				let data = self.arena() <- LetData { pattern: Late::new(), value, then: Late::new() };
				let n_added = {
					//self.start_check_pattern(value.ty(), pattern_ast);
					let &ast::Pattern(pattern_loc, ref pattern_data) = pattern_ast;
					match *pattern_data {
						ast::PatternData::Ignore => {
							&data.pattern <- Pattern::Ignore;
							0
						}
						ast::PatternData::Single(name) => {
							let pattern = &data.pattern <- Pattern::Single(Local {
								loc: pattern_loc,
								ty: data.value.ty.clone(),
								name,
							});
							self.add_to_scope(match *pattern {
								Pattern::Single(ref local) => local,
								_ => unreachable!(),
							}); //TODO:neater
							1
						}
						ast::PatternData::Destruct(_) => unimplemented!(),
					}
				};
				&data.then <- self.check_expr(expected, then_ast);
				for _ in 0..n_added {
					self.pop_from_scope()
				}
				self.handled(expected, loc, ExprData::Let(data))
			}
			ast::ExprData::Seq(&ast::SeqData(ref first_ast, ref then_ast)) => {
				let first = self.check_void(first_ast);
				let then = self.check_expr(expected, then_ast);
				let data = ExprData::Seq(self.arena() <- SeqData(first, then));
				self.handled(expected, loc, data)
			}
			ast::ExprData::LiteralNat(n) =>
				self.handle(expected, loc, self.ctx.builtins.nat.clone(), ExprData::LiteralNat(n)),
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
				let ty = Ty::Plain(
					PlainTy { effect: self.self_effect, inst_class: self.ctx.current_inst_class() },
				);
				self.handle(expected, loc, ty, ExprData::SelfExpr)
			}
			ast::ExprData::IfElse(&ast::IfElseData(ref test_ast, ref then_ast, ref else_ast)) => {
				let test = self.check_bool(test_ast);
				let then = self.check_expr(expected, then_ast);
				let elze = self.check_expr(expected, else_ast);
				let data = self.arena() <- IfElseData { test, then, elze };
				self.handled(expected, loc, ExprData::IfElse(data))
			}
			ast::ExprData::WhenTest(&ast::WhenTestData { cases: case_asts, elze: ref else_ast }) => {
				let cases =
					self.arena()
						.map(case_asts, |&ast::Case(case_loc, ref test_ast, ref result_ast)| {
							let test = self.check_bool(test_ast);
							let result = self.check_expr(expected, result_ast);
							Case { loc: case_loc, test, result }
						});
				let elze = self.check_expr(expected, else_ast);
				let data = self.arena() <- WhenTestData { cases, elze };
				self.handled(expected, loc, ExprData::WhenTest(data))
			}
			ast::ExprData::Assert(asserted_ast) => {
				let asserted = self.arena() <- self.check_bool(asserted_ast);
				self.handle(expected, loc, self.ctx.builtins.void.clone(), ExprData::Assert(asserted))
			}
			ast::ExprData::Try(
				&ast::TryData { try: ref do_ast, catch: ref catch_ast, finally: ref op_finally_ast },
			) => {
				let body = self.check_expr(expected, do_ast);
				let catch_init = catch_ast.as_ref().map(
					|&ast::Catch {
					     loc: catch_loc,
					     exception_ty: ref exception_ty_ast,
					     exception_name_loc,
					     exception_name,
					     result: _,
					 }| {
						let exception_ty = self.ctx.get_ty(exception_ty_ast);
						Catch {
							loc: catch_loc,
							caught: Local { loc: exception_name_loc, ty: exception_ty, name: exception_name },
							result: Late::new(),
						}
					},
				);
				let finally = op_finally_ast.as_ref().map(|f| self.check_void(f));
				let data = self.arena() <- TryData { body, catch: catch_init, finally };
				if let Some(catch) = data.catch.as_ref() {
					self.add_to_scope(&catch.caught);
					let &ast::Catch { result: ref result_ast, .. } = catch_ast.as_ref().unwrap();
					&catch.result <- self.check_expr(expected, result_ast);
					self.pop_from_scope();
				}
				self.handled(expected, loc, ExprData::Try(data))
			}
			ast::ExprData::For(
				&ast::ForData { local_name: _, looper: ref _looper_ast, body: ref _body_ast },
			) => unimplemented!(),
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

	fn call_static_method<'ast, 'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		loc: Loc,
		class: Up<'model, ClassDeclaration<'model>>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let infer_arena = Arena::new();
		let method_and_inferrer = O(
			self.ctx
				.get_static_method(loc, class, method_name, &infer_arena),
		)?;
		let (inst_method, args) =
			O(self.check_call_common(expected, loc, method_and_inferrer, ty_arg_asts, arg_asts))?;
		let data = self.arena() <- StaticMethodCallData { method: inst_method, args };
		self.handled(expected, loc, ExprData::StaticMethodCall(data))
	}

	fn call_my_method<'ast, 'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		loc: Loc,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: List<'ast, ast::Expr<'ast>>,
	) -> Handled<'model> {
		let infer_arena = Arena::new();
		let method_and_inferrer = O(
			self.ctx
				.get_own_method(loc, self.self_effect, method_name, &infer_arena),
		)?;
		let method_decl = method_and_inferrer.0;

		let (inst_method, args) =
			O(self.check_call_common(expected, loc, method_and_inferrer, ty_arg_asts, arg_asts))?;

		let data = if method_decl.is_static() {
			// OK to access my static method in my instance method.
			ExprData::StaticMethodCall(self.arena() <- StaticMethodCallData { method: inst_method, args })
		} else {
			if self.is_static {
				self.add_diagnostic(loc, Diag::CantCallInstanceMethodFromStaticMethod(method_decl));
				return Handled(None)
			}
			ExprData::MyInstanceMethodCall(
				self.arena() <- MyInstanceMethodCallData { method: inst_method, args },
			)
		};

		self.handled(expected, loc, data)
	}

	fn call_instance_method<'ast, 'infer, I: KnownLen<Item = &'ast ast::Expr<'ast>>>(
		&mut self,
		expected: Expected<'infer, 'model>,
		loc: Loc,
		target_ast: &'ast ast::Expr<'ast>,
		method_name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: I,
	) -> Handled<'model> {
		let target = self.check_infer(target_ast);
		let infer_arena = Arena::new();
		let method_and_inferrer = O(
			self.ctx
				.get_method_from_ty(loc, &target.ty, method_name, &infer_arena),
		)?;

		if let MethodOrImplOrAbstract::Method(m) = method_and_inferrer.0 {
			if m.is_static {
				self.add_diagnostic(loc, Diag::CantAccessStaticMethodThroughInstance(m.clone_as_up()));
				return Handled(None)
			}
		}

		let (inst_method, args) =
			O(self.check_call_common(expected, loc, method_and_inferrer, ty_arg_asts, arg_asts))?;
		let data = self.arena() <- InstanceMethodCallData { target, method: inst_method, args };
		self.handled(expected, loc, ExprData::InstanceMethodCall(data))
	}

	//mv
	fn instantiate_ty(&mut self, ty: &Ty<'model>, instantiator: &Instantiator<'model>) -> Ty<'model> {
		instantiate_ty(ty, instantiator, self.arena())
	}

	fn get_my_slot<'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		loc: Loc,
		slot: Up<'model, SlotDeclaration<'model>>,
	) -> Handled<'model> {
		if self.is_static {
			self.add_diagnostic(loc, Diag::CantAccessSlotFromStaticMethod(slot));
			return Handled(None)
		}

		if slot.mutable && !self.self_effect.can_get() {
			self.add_diagnostic(loc, Diag::MissingEffectToGetSlot(slot))
		}

		let slot_ty = instantiate_and_narrow_effects(
			self.self_effect,
			&slot.ty,
			&Instantiator::NIL,
			loc,
			&mut self.ctx.diags,
			self.ctx.arena,
		);
		self.handle(expected, loc, slot_ty, ExprData::GetMySlot(slot))
	}

	fn handled<'infer>(
		&mut self,
		expected: Expected<'infer, 'model>,
		loc: Loc,
		data: ExprData<'model>,
	) -> Handled<'model> {
		handled(Expr { loc, ty: expected.inferred().clone(), data })
	}

	fn check_call_common<'ast, 'outer_infer, 'infer, I : KnownLen<Item=&'ast ast::Expr<'ast>>>(
		&mut self,
		Expected {
			in_tail_call_position: _, expected_ty: expected_return_ty_cell, inferrer: return_ty_inferrer
		}: Expected<'outer_infer, 'model>,
		loc: Loc,
		// For a method on an instantiated generic target, this will be pre-filled with its type arguments.
		MethodAndInferrer(method, inferrer): MethodAndInferrer<'infer, 'model>,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
		arg_asts: I
) -> Option<(InstMethod<'model>, &'model [Expr<'model>])>{
		let expected_return_ty_ref: &mut Option<Ty<'model>> =
			unsafe { expected_return_ty_cell.get().as_mut().unwrap() };

		if !ty_arg_asts.is_empty() {
			//Infer everything from these.
			unimplemented!()
		}

		// If available, we do inference with expected first.
		if let Some(ref expected_return_ty) = *expected_return_ty_ref {
			let is_assignable = check_assignable(
				expected_return_ty,
				method.return_ty(),
				&return_ty_inferrer.combine(&inferrer),
				self.arena(),
			);
			if !is_assignable {
				self.add_diagnostic(
					loc,
					Diag::NotAssignable {
						expected: expected_return_ty.clone(),
						actual: method.return_ty().clone(),
					},
				);
				return None
			}
		}

		if method.parameters().len() != arg_asts.len() {
			self.add_diagnostic(loc, Diag::ArgumentCountMismatch(method, arg_asts.len()));
			return None
		}

		//Now do each arg in turn.
		let args = self.arena()
			.map(method.parameters().zip(arg_asts), |(param, arg_ast)| {
				let expected_ty = UnsafeCell::new(Some(param.ty.clone()));
				let expected =
					Expected { in_tail_call_position: false, expected_ty: &expected_ty, inferrer: &inferrer };
				self.check_expr(expected, arg_ast)
			});

		if let None = *expected_return_ty_ref {
			// If we didn't have an expected ty coming in, fill it in now.
			*expected_return_ty_ref = Some(instantiate_ty(method.return_ty(), &inferrer, self.arena()))
		}

		Some((InstMethod::new(method, inferrer.into_ty_args()), args))
	}

	fn handle<'infer>(
		&mut self,
		Expected { in_tail_call_position: _, expected_ty: cell, inferrer }: Expected<'infer, 'model>,
		loc: Loc,
		actual_ty: Ty<'model>,
		data: ExprData<'model>,
	) -> Handled<'model> {
		let expected_ty: &mut Option<Ty<'model>> = unsafe { cell.get().as_mut().unwrap() };
		match *expected_ty {
			Some(ref ty) => {
				let is_assignable = check_assignable(ty, &actual_ty, inferrer, self.arena());
				if !is_assignable {
					self.add_diagnostic(
						loc,
						Diag::NotAssignable { expected: ty.clone(), actual: actual_ty.clone() },
					)
				}
			}
			None => *expected_ty = Some(actual_ty.clone()),
		}
		handled(Expr { loc, ty: actual_ty, data })
	}
}


const NOPE: Handled<'static> = Handled(None); //TODO:use
fn handled<'model>(e: Expr<'model>) -> Handled<'model> {
	Handled(Some(e))
}

//TODO:NEATER
struct Handled<'model>(Option<Expr<'model>>);
impl<'model> Try for Handled<'model> {
	type Ok = Expr<'model>;
	type Error = ();

	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		match self.0 {
			Some(t) => Ok(t),
			None => Err(()),
		}
	}

	fn from_ok(v: Self::Ok) -> Self {
		handled(v)
	}

	fn from_error((): Self::Error) -> Self {
		Handled(None)
	}
}

//TODO: remove when Option implements Try
struct O<T>(Option<T>);
impl<T> Try for O<T> {
	type Ok = T;
	type Error = ();

	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		match self.0 {
			Some(t) => Ok(t),
			None => Err(()),
		}
	}

	fn from_ok(v: Self::Ok) -> Self {
		O(Some(v))
	}

	fn from_error((): Self::Error) -> Self {
		O(None)
	}
}
