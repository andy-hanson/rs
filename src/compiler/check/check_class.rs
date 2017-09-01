use util::arith::to_u8;
use util::arr::Arr;
use util::ptr::{LateOwn, Own};
use util::sym::Sym;

use super::super::compile::builtins::BuiltinsCtx;
use super::super::diag::Diag;
use super::super::model::class::{ClassDeclaration, ClassHead, SlotDeclaration, Super};
use super::super::model::method::{Impl, MethodOrImpl, MethodSignature, MethodWithBody, Parameter};
use super::super::model::module::Module;
use super::super::model::ty::{TypeParameter, TypeParameterOrigin};
use super::super::parse::ast;

use super::check_expr::check_method_body;
use super::ctx::Ctx;
use super::instantiator::Instantiator;

pub fn check_module(module: &Module, builtins: &BuiltinsCtx, ast: &ast::Class, name: Sym) {
	let type_parameters = ast.type_parameters.map_on_copies(TypeParameter::create);
	let class = &module.class;
	// Create the class early and assign its properties later.
	// This allows us to access the class' type when checking type annotations.
	class.init(ClassDeclaration {
		type_parameters,
		name,
		head: LateOwn::new(),
		supers: LateOwn::new(),
		methods: LateOwn::new(),
	});
	let origin = TypeParameterOrigin::Class(class.ptr());
	TypeParameter::set_origins(&class.type_parameters, origin);
	let mut ctx = Ctx::new(class, builtins, &module.imports);
	do_check(&mut ctx, ast);
	module.diagnostics.init(ctx.finish())
}

fn do_check(ctx: &mut Ctx, ast: &ast::Class) {
	// type parameters already handled before calling this.
	let &ast::Class { head: ref head_ast, supers: ref super_asts, methods: ref method_asts, .. } = ast;

	let methods = method_asts.map(|m| check_method_initial(ctx, m));
	ctx.current_class.methods.init(methods);

	// Adds slots too
	let head = check_head(ctx, head_ast.as_ref());
	ctx.current_class.head.init(head);

	let supers = super_asts.map_defined_probably_all(|s| check_super_initial(ctx, s));
	ctx.current_class.supers.init(supers);

	// We delayed checking impl bodies until now
	// because we need all supers present for method resolution.
	fill_impl_bodies(ctx, super_asts);
	fill_method_bodies(ctx, method_asts);
}

//mv
fn fill_impl_bodies(ctx: &mut Ctx, super_asts: &Arr<ast::Super>) {
	// There may be fewer supers than super_asts.
	// TODO: but we can check that they correspond using the `loc`.

	let supers = &ctx.current_class.supers;
	if super_asts.len() != supers.len() {
		todo!()
	}

	super_asts.do_zip(supers, |super_ast, zuper| {
		let instantiator = Instantiator::of_inst_cls(&zuper.super_class);
		super_ast.impls.do_zip(&zuper.impls, |impl_ast, real_impl| {
			let body = match impl_ast.body {
				Some(ref body_ast) =>
					Some(check_method_body(
						ctx,
						&MethodOrImpl::Impl(real_impl.ptr()),
						&instantiator,
						/*is_static*/ false,
						body_ast,
					)),
				None => None,
			};
			real_impl.body.init(body)
		})
	})
}

fn fill_method_bodies(ctx: &mut Ctx, method_asts: &Arr<ast::Method>) {
	// Now that all methods exist, fill in their bodies.
	for i in method_asts.range() {
		let method_ast = &method_asts[i];
		let method = ctx.current_class.methods[i].ptr();
		let body = match method_ast.body {
			Some(ref body_ast) =>
				Some(check_method_body(
					ctx,
					&MethodOrImpl::Method(method.clone_ptr()),
					&Instantiator::nil(),
					method.is_static,
					body_ast,
				)),
			None => None,
		};
		method.set_body(body)
	}
}

fn check_super_initial(
	ctx: &mut Ctx,
	&ast::Super { loc, name, ref ty_args, impls: ref impl_asts }: &ast::Super,
) -> Option<Super> {
	//let super_inst_cls = unwrap_or_return!(ctx.instantiate_class_from_ast(loc, name, ty_args), None);
	//let super_class_declaration = super_inst_cls.class();
	let super_class_declaration =
		unwrap_or_return!(ctx.access_class_declaration_or_add_diagnostic(loc, name), None);
	if super_class_declaration.supers.len() != 0 {
		// We should have a check that there is a separate implementation of the super-super.
		todo!()
	}

	let impls = {
		let abstract_methods = match *super_class_declaration.head {
			ClassHead::Abstract(_, ref abstract_methods) => abstract_methods,
			_ => {
				ctx.add_diagnostic(loc, Diag::NotAnAbstractClass(super_class_declaration.clone_ptr()));
				return None
			}
		};

		if !abstract_methods
			.each_corresponds(impl_asts, |implemented, impl_ast| implemented.name() == impl_ast.name)
		{
			ctx.add_diagnostic(
				loc,
				Diag::ImplsMismatch { expected_names: abstract_methods.map(|a| a.name()) },
			);
			return None
		}
		abstract_methods.zip(impl_asts, |implemented, &ast::Impl { loc, ref parameter_names, .. }| {
			if !implemented
				.parameters()
				.each_corresponds(parameter_names, |p, pn| p.name == *pn)
			{
				ctx.add_diagnostic(loc, Diag::WrongImplParameters(implemented.ptr()));
				todo!() // Should we continue or what?
			}

			Own::new(Impl { loc, implemented: implemented.ptr(), body: LateOwn::new() })
		})
	};

	let super_inst_cls = unwrap_or_return!(ctx.instantiate_class(super_class_declaration, ty_args), None);
	Some(Super { loc, super_class: super_inst_cls, impls })
}

fn check_method_initial(ctx: &mut Ctx, ast: &ast::Method) -> Own<MethodWithBody> {
	// Don't check method bodies yet, just fill their heads.
	let &ast::Method {
		loc,
		is_static,
		type_parameters: ref type_parameter_asts,
		return_ty: ref return_ty_ast,
		name,
		self_effect,
		parameters: ref parameter_asts,
		..
	} = ast;
	let type_parameters = type_parameter_asts.map_on_copies(TypeParameter::create);
	let return_ty = ctx.get_ty_or_type_parameter(return_ty_ast, &type_parameters);
	let parameters = check_parameters(ctx, parameter_asts, &type_parameters);
	let method = Own::new(MethodWithBody {
		signature: MethodSignature {
			class: ctx.current_class.ptr(),
			loc,
			name,
			type_parameters,
			return_ty,
			self_effect,
			parameters,
		},
		is_static,
		body: LateOwn::new(),
	});
	TypeParameter::set_origins(method.type_parameters(), TypeParameterOrigin::Method(method.ptr()));
	method
}

fn check_parameters(
	ctx: &mut Ctx,
	param_asts: &Arr<ast::Parameter>,
	type_parameters: &Arr<Own<TypeParameter>>,
) -> Arr<Own<Parameter>> {
	param_asts.map_with_index(|&ast::Parameter { loc, ty: ref ty_ast, name }, index| {
		for prior_param in param_asts.iter().take(index) {
			if prior_param.name == name {
				todo!()
				//ctx.add_diagnostic(loc, )
			}
		}
		Own::new(Parameter {
			loc,
			ty: ctx.get_ty_or_type_parameter(ty_ast, type_parameters),
			name,
			index: to_u8(index),
		})
	})
}

fn check_head(ctx: &mut Ctx, ast: Option<&ast::ClassHead>) -> ClassHead {
	let &ast::ClassHead(loc, ref head_data) = unwrap_or_return!(ast, {
		if ctx.current_class.type_parameters.any() {
			todo!() // Error: static class can't have type parameters
		}
		ClassHead::Static
	});
	match *head_data {
		ast::ClassHeadData::Abstract(_) => {
			unused!(loc);
			todo!()
		}
		ast::ClassHeadData::Slots(_) => todo!(),
		ast::ClassHeadData::Builtin => ClassHead::Builtin,
	}
}

fn check_slot(ctx: &mut Ctx, slot_ast: &ast::Slot) -> SlotDeclaration {
	unused!(ctx, slot_ast);
	todo!()
}
