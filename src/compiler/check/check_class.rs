use util::arena::{Up, Arena, List};
use util::arith::to_u8;
use util::arr::{SliceOps};
use util::late::Late;
use util::sym::Sym;

use super::super::super::model::class::{ClassDeclaration, ClassHead, SlotDeclaration, Super};
use super::super::super::model::diag::Diag;
use super::super::super::model::method::{AbstractMethod, Impl, MethodOrImpl, MethodSignature, MethodWithBody, Parameter};
use super::super::super::model::module::Module;
use super::super::super::model::ty::{TypeParameter, TypeParameterOrigin};

use super::super::builtins::BuiltinsCtx;
use super::super::parse::ast;

use super::check_expr::check_method_body;
use super::ctx::Ctx;
use super::instantiator::Instantiator;

pub fn check_module<'ast, 'builtins_ctx, 'model>(module: &'model Module<'model>, builtins: &'builtins_ctx BuiltinsCtx<'model>, ast: &'ast ast::Class<'ast>, name: Sym, arena: &'model Arena) {
	let type_parameters: &'model [TypeParameter<'model>] = ast.type_parameters.map(arena, |name| TypeParameter::create(*name));
	// Create the class early and assign its properties later.
	// This allows us to access the class' type when checking type annotations.
	let class: &'model ClassDeclaration<'model> = module.class.init(ClassDeclaration {
		type_parameters,
		name,
		head: Late::new(),
		supers: Late::new(),
		methods: Late::new(),
	});
	TypeParameter::set_origins(class.type_parameters, TypeParameterOrigin::Class(Up(class)));
	let mut ctx: Ctx<'builtins_ctx, 'model> = Ctx::new(class, builtins, module.imports, arena);
	do_check(&mut ctx, ast);
	module.diagnostics.init(ctx.finish());
}

fn do_check<'ast, 'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, ast: &'ast ast::Class<'ast>) {
	// type parameters already handled before calling this.
	let &ast::Class { head: ref head_ast, supers: ref super_asts, methods: ref method_asts, .. } = ast;

	let methods = method_asts.map(ctx.arena, |m| check_method_initial(ctx, m));
	ctx.current_class.methods.init(methods);

	// Adds slots too
	let head = check_head(ctx, head_ast.as_ref());
	ctx.current_class.head.init(head);

	let supers: &'model [Super<'model>] = super_asts.map_defined_probably_all(ctx.arena, |s| check_super_initial(ctx, s));
	ctx.current_class.supers.init(supers);

	// We delayed checking impl bodies until now
	// because we need all supers present for method resolution.
	fill_impl_bodies(ctx, super_asts);
	fill_method_bodies(ctx, method_asts);
}

//mv
fn fill_impl_bodies<'ast, 'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, super_asts: &'ast List<'ast, ast::Super<'ast>>) {
	// There may be fewer supers than super_asts.
	// TODO: but we can check that they correspond using the `loc`.

	let supers = &ctx.current_class.supers;
	if super_asts.len != supers.len() {
		unimplemented!()
	}

	super_asts.do_zip(supers, |super_ast, zuper| {
		let instantiator = Instantiator::of_inst_cls(&zuper.super_class);
		let impl_asts = &super_ast.impls;
		impl_asts.do_zip(zuper.impls, |impl_ast, real_impl| {
			let body = match impl_ast.body {
				Some(body_ast) =>
					Some(check_method_body(
						ctx,
						MethodOrImpl::Impl(Up(real_impl)),
						&instantiator,
						/*is_static*/ false,
						body_ast,
					)),
				None => None,
			};
			real_impl.body.init(body);
		})
	})
}

fn fill_method_bodies<'ast>(ctx: &mut Ctx, method_asts: &'ast List<'ast, ast::Method<'ast>>) {
	// Now that all methods exist, fill in their bodies.
	for (i, method_ast) in method_asts.iter().enumerate() {
		let method = &ctx.current_class.methods[i];
		let body = match method_ast.body {
			Some(body_ast) =>
				Some(check_method_body(
					ctx,
					MethodOrImpl::Method(Up(method)),
					&Instantiator::NIL,
					method.is_static,
					body_ast,
				)),
			None => None,
		};
		method.body.init(body);
	}
}

fn check_super_initial<'ast, 'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	&ast::Super { loc, name, ref ty_args, impls: ref impl_asts }: &'ast ast::Super<'ast>,
) -> Option<Super<'model>> {
	//let super_inst_cls = unwrap_or_return!(ctx.instantiate_class_from_ast(loc, name, ty_args), None);
	//let super_class_declaration = super_inst_cls.class();
	let super_class_declaration =
		unwrap_or_return!(ctx.access_class_declaration_or_add_diagnostic(loc, name), None);
	if super_class_declaration.supers.len() != 0 {
		// We should have a check that there is a separate implementation of the super-super.
		unimplemented!()
	}

	let impls = {
		let abstract_methods = match *super_class_declaration.head {
			ClassHead::Abstract(_, abstract_methods) => abstract_methods,
			_ => {
				ctx.add_diagnostic(loc, Diag::NotAnAbstractClass(Up(super_class_declaration)));
				return None
			}
		};

		if !abstract_methods
			.each_corresponds_list(impl_asts, |implemented, impl_ast| implemented.name() == impl_ast.name)
		{
			ctx.add_diagnostic(
				loc,
				Diag::ImplsMismatch { expected_names: abstract_methods.map(ctx.arena, |a| a.name()) },
			);
			return None
		}
		abstract_methods.zip(impl_asts.iter(), ctx.arena, |implemented, &ast::Impl { loc, parameter_names, .. }| {
			let x: &'model AbstractMethod<'model> = implemented;
			if !implemented
				.parameters()
				.each_corresponds(parameter_names, |p, pn| p.name == *pn)
			{
				ctx.add_diagnostic(loc, Diag::WrongImplParameters(Up(x)));
				unimplemented!() // Should we continue or what?
			}

			Impl { loc, implemented: Up(implemented), body: Late::new() }
		})
	};

	let super_inst_cls = unwrap_or_return!(ctx.instantiate_class(super_class_declaration, ty_args), None);
	Some(Super { loc, super_class: super_inst_cls, impls })
}

//TODO:PERF would like to return by value...
fn check_method_initial<'ast, 'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, ast: &'ast ast::Method<'ast>) -> &'model MethodWithBody<'model> {
	// Don't check method bodies yet, just fill their heads.
	let &ast::Method {
		loc,
		is_static,
		type_parameters: type_parameter_asts,
		return_ty: ref return_ty_ast,
		name,
		self_effect,
		parameters: ref parameter_asts,
		..
	} = ast;
	let type_parameters = type_parameter_asts.map(ctx.arena, |name| TypeParameter::create(*name));
	let return_ty = ctx.get_ty_or_type_parameter(return_ty_ast, type_parameters);
	let parameters = check_parameters(ctx, parameter_asts, type_parameters);
	let method = ctx.arena <- MethodWithBody {
		signature: MethodSignature {
			class: Up(ctx.current_class),
			loc,
			name,
			type_parameters,
			return_ty,
			self_effect,
			parameters,
		},
		is_static,
		body: Late::new(),
	};
	TypeParameter::set_origins(method.type_parameters(), TypeParameterOrigin::Method(Up(method)));
	method
}

fn check_parameters<'builtins_ctx, 'ast, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	param_asts: &'ast List<'ast, ast::Parameter<'ast>>,
	type_parameters: &[TypeParameter<'model>],
) -> &'model [Parameter<'model>] {
	param_asts.map_with_index(ctx.arena, |&ast::Parameter { loc, ty: ref ty_ast, name }, index| {
		for prior_param in param_asts.iter().take(index) {
			if prior_param.name == name {
				unimplemented!()
				//ctx.add_diagnostic(loc, )
			}
		}
		Parameter {
			loc,
			ty: ctx.get_ty_or_type_parameter(ty_ast, type_parameters),
			name,
			index: to_u8(index),
		}
	})
}

fn check_head<'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, ast: Option<&ast::ClassHead>) -> ClassHead<'model> {
	let &ast::ClassHead(loc, ref head_data) = unwrap_or_return!(ast, {
		if ctx.current_class.type_parameters.any() {
			unimplemented!() // Error: static class can't have type parameters
		}
		ClassHead::Static
	});
	match *head_data {
		ast::ClassHeadData::Abstract(_) => {
			unused!(loc);
			unimplemented!()
		}
		ast::ClassHeadData::Slots(_) => unimplemented!(),
		ast::ClassHeadData::Builtin => ClassHead::Builtin,
	}
}

fn check_slot<'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, slot_ast: &ast::Slot) -> SlotDeclaration<'model> {
	unused!(ctx, slot_ast);
	unimplemented!()
}
