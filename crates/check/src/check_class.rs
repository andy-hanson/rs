use util::arena::Arena;
use util::arith::to_u8;
use util::iter::KnownLen;
use util::late::Late;
use util::list::List;
use util::sym::Sym;
use util::up::Up;

use ast;

use model::builtins::BuiltinsOwn;
use model::class::{ClassDeclaration, ClassHead, SlotDeclaration, Super};
use model::diag::Diag;
use model::method::{AbstractMethod, Impl, MethodOrImpl, MethodSignature, MethodWithBody, Parameter};
use model::module::Module;
use model::ty::{TypeParameter, TypeParameterOrigin};

use super::ast_utils::effect_to_effect;
use super::check_expr::check_method_body;
use super::ctx::Ctx;
use super::instantiator::Instantiator;

pub fn check_module<'ast, 'builtins_ctx, 'model>(
	module: &'model Module<'model>,
	builtins: &'builtins_ctx BuiltinsOwn<'model>,
	ast: &'ast ast::Class<'ast>,
	name: Sym,
	arena: &'model Arena,
) {
	let type_parameters: &'model [TypeParameter<'model>] =
		arena.map(ast.type_parameters, |name| TypeParameter::create(*name));
	// Create the class early and assign its properties later.
	// This allows us to access the class' type when checking type annotations.
	let class: &'model ClassDeclaration<'model> = &module.class <- ClassDeclaration {
		type_parameters,
		name,
		head: Late::new(),
		supers: Late::new(),
		methods: Late::new(),
	};
	TypeParameter::set_origins(class.type_parameters, TypeParameterOrigin::Class(Up(class)));
	let mut ctx: Ctx<'builtins_ctx, 'model> = Ctx::new(Up(class), builtins, module.imports, arena);
	do_check(&mut ctx, ast);
	&module.diagnostics <- ctx.finish();
}

fn do_check<'ast, 'builtins_ctx, 'model>(ctx: &mut Ctx<'builtins_ctx, 'model>, ast: &'ast ast::Class<'ast>) {
	// type parameters already handled before calling this.
	let &ast::Class { head: ref head_ast, supers: super_asts, methods: method_asts, .. } = ast;

	let methods = ctx.arena.map(method_asts, |m| check_method_initial(ctx, m));
	&ctx.current_class.methods <- methods;

	// Adds slots too
	let head = check_head(ctx, head_ast.as_ref());
	&ctx.current_class.head <- head;

	let supers = ctx.arena
		.map_defined_probably_all(super_asts, |s| check_super_initial(ctx, s));
	&ctx.current_class.supers <- supers;

	// We delayed checking impl bodies until now
	// because we need all supers present for method resolution.
	fill_impl_bodies(ctx, super_asts);
	fill_method_bodies(ctx, method_asts);
}

//mv
fn fill_impl_bodies<'ast, 'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	super_asts: List<'ast, ast::Super<'ast>>,
) {
	// There may be fewer supers than super_asts.
	// TODO: but we can check that they correspond using the `loc`.

	let supers = *ctx.current_class.supers;
	if super_asts.len() != supers.len() {
		unimplemented!()
	}

	for (super_ast, zuper) in super_asts.zip(supers) {
		let instantiator = Instantiator::of_inst_cls(&zuper.super_class);
		let impl_asts = &super_ast.impls;
		for (impl_ast, real_impl) in impl_asts.zip(zuper.impls) {
			let body = match impl_ast.body {
				Some(ref body_ast) =>
					Some(check_method_body(
						ctx,
						MethodOrImpl::Impl(Up(real_impl)),
						&instantiator,
						/*is_static*/ false,
						body_ast,
					)),
				None => None,
			};
			&real_impl.body <- body;
		}
	}
}

fn fill_method_bodies<'ast>(ctx: &mut Ctx, method_asts: List<'ast, ast::Method<'ast>>) {
	// Now that all methods exist, fill in their bodies.
	for (i, method_ast) in method_asts.iter().enumerate() {
		let method = &ctx.current_class.methods[i];
		let body = match method_ast.body {
			Some(ref body_ast) =>
				Some(check_method_body(
					ctx,
					MethodOrImpl::Method(Up(method)),
					&Instantiator::NIL,
					method.is_static,
					body_ast,
				)),
			None => None,
		};
		&method.body <- body;
	}
}

fn check_super_initial<'ast, 'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	&ast::Super { loc, name, ty_args, impls: impl_asts }: &'ast ast::Super<'ast>,
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
				ctx.add_diagnostic(loc, Diag::NotAnAbstractClass(super_class_declaration));
				return None
			}
		};

		if !abstract_methods
			.each_corresponds(impl_asts, |implemented, impl_ast| implemented.name() == impl_ast.name)
		{
			ctx.add_diagnostic(loc, Diag::ImplsMismatch { expected: abstract_methods });
			return None
		}
		ctx.arena.map(
			abstract_methods.zip(impl_asts),
			|(implemented, &ast::Impl { loc, parameter_names, .. })| {
				let x: &'model AbstractMethod<'model> = implemented;
				if !implemented
					.parameters()
					.each_corresponds(parameter_names, |p, pn| p.name == *pn)
				{
					ctx.add_diagnostic(loc, Diag::WrongImplParameters(Up(x)));
					unimplemented!() // Should we continue or what?
				}

				Impl { loc, containing_class: ctx.current_class, implemented: Up(implemented), body: Late::new() }
			},
		)
	};

	let super_inst_cls = unwrap_or_return!(ctx.instantiate_class(super_class_declaration, ty_args), None);
	Some(Super { loc, super_class: super_inst_cls, impls })
}

//TODO:PERF would like to return by value...
fn check_method_initial<'ast, 'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	ast: &'ast ast::Method<'ast>,
) -> &'model MethodWithBody<'model> {
	// Don't check method bodies yet, just fill their heads.
	let &ast::Method {
		loc,
		is_static,
		type_parameters: type_parameter_asts,
		return_ty: ref return_ty_ast,
		name,
		self_effect,
		parameters: parameter_asts,
		..
	} = ast;
	let type_parameters = ctx.arena
		.map(type_parameter_asts, |name| TypeParameter::create(*name));
	let return_ty = ctx.get_ty_or_ty_parameter(return_ty_ast, type_parameters);
	let parameters = check_parameters(ctx, parameter_asts, type_parameters);
	let method = ctx.arena <- MethodWithBody {
		containing_class: ctx.current_class,
		signature: MethodSignature {
			class: ctx.current_class,
			loc,
			name,
			type_parameters,
			return_ty,
			self_effect: effect_to_effect(self_effect),
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
	param_asts: List<'ast, ast::Parameter<'ast>>,
	type_parameters: &'model [TypeParameter<'model>],
) -> &'model [Parameter<'model>] {
	ctx.arena
		.map(param_asts.enumerate(), |(index, &ast::Parameter { loc, ty: ref ty_ast, name })| {
			for prior_param in param_asts.iter().take(index) {
				if prior_param.name == name {
					unimplemented!()
					//ctx.add_diagnostic(loc, )
				}
			}
			Parameter {
				loc,
				ty: ctx.get_ty_or_ty_parameter(ty_ast, type_parameters),
				name,
				index: to_u8(index),
			}
		})
}

fn check_head<'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	ast: Option<&ast::ClassHead>,
) -> ClassHead<'model> {
	let &ast::ClassHead(loc, ref head_data) = unwrap_or_return!(ast, {
		if !ctx.current_class.type_parameters.is_empty() {
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

fn check_slot<'builtins_ctx, 'model>(
	ctx: &mut Ctx<'builtins_ctx, 'model>,
	slot_ast: &ast::Slot,
) -> SlotDeclaration<'model> {
	unused!(ctx, slot_ast);
	unimplemented!()
}
