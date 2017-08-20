use util::arr::{ Arr };
use util::ptr::{ Own, Ptr };
use util::sym::Sym;

use super::super::diag::{ Diagnostic };
use super::super::model::class::{ ClassDeclaration, ClassHead, SlotDeclaration, Super };
use super::super::model::method::{ MethodWithBody, MethodOrImpl, Parameter };
use super::super::model::module::{ Imported };
use super::super::model::ty::{ TypeParameter, TypeParameterOrigin };
use super::super::parse::ast;

use super::ctx::Ctx;
use super::check_expr::check_method_body;
use super::ty_replacer::TyReplacer;

pub fn check_class(
	imports: Arr<Imported>,
	ast: ast::ClassDeclaration,
	name: Sym
	) -> (Own<ClassDeclaration>, Arr<Diagnostic>) {

	let type_parameters = ast.type_parameters.map_on_copies(TypeParameter::create);
	// Create the class early and assign its properties later.
	// This allows us to access the class' type when checking type annotations.
	let current_class = Own::new(ClassDeclaration::new(name, type_parameters));
	let origin = TypeParameterOrigin::Class(current_class.ptr());
	TypeParameter::set_origins(&current_class.type_parameters, origin);
	let mut ctx = Ctx::new(current_class, imports);
	do_check(&mut ctx, &ast);
	ctx.finish()
}

fn do_check(ctx: &mut Ctx, ast: &ast::ClassDeclaration) {
	let &ast::ClassDeclaration {
		// type parameters handled before calling this.
		loc: _, type_parameters: _, head: ref head_ast, supers: ref super_asts, methods: ref method_asts
	} = ast; // Already used type parameters

	let methods = method_asts.map(|m| check_method_initial(ctx, m));
	ctx.current_class.set_methods(methods);

	// Adds slots too
	let head = check_head(ctx, head_ast.as_ref());
	ctx.current_class.set_head(head);

	let supers = super_asts.map_defined_probably_all(|super_ast| check_super(ctx, super_ast));
	ctx.current_class.set_supers(supers);

	// Now that all methods exist, fill in their bodies.
	for i in method_asts.range() {
		let method_ast = &method_asts[i];
		let method = ctx.current_class.methods()[i].ptr();
		set_method_body(ctx, method, &TyReplacer::do_nothing(), &method_ast.body);
	}
}

fn check_method_initial(ctx: &mut Ctx, ast: &ast::Method) -> Own<MethodWithBody> {
	// Don't check method bodies yet, just fill their heads.
	let &ast::Method { loc, is_static, type_parameters: ref type_parameter_asts, return_ty: ref return_ty_ast, name, self_effect,
		parameters: ref parameter_asts, body: _ } = ast;
	let type_parameters = type_parameter_asts.map_on_copies(TypeParameter::create);
	let return_ty = ctx.get_ty_or_type_parameter(&return_ty_ast, &type_parameters);
	let parameters = check_parameters(ctx, parameter_asts, &type_parameters);
	let method = Own::new(MethodWithBody::new(
		&ctx.current_class, loc, is_static, name, type_parameters, return_ty, self_effect, parameters));
	TypeParameter::set_origins(&method.type_parameters(), TypeParameterOrigin::Method(method.ptr()));
	method
}

fn check_parameters(ctx: &mut Ctx, param_asts: &Arr<ast::Parameter>, type_parameters: &Arr<Own<TypeParameter>>) -> Arr<Own<Parameter>> {
	param_asts.map_with_index(|&ast::Parameter { loc, ty: ref ty_ast, name }, index| {
		for j in 0..index {
			if param_asts[j].name == name {
				panic!()
				//ctx.add_diagnostic(loc, )
			}
		}
		Own::new(Parameter { loc, ty: ctx.get_ty_or_type_parameter(ty_ast, type_parameters), name, index: index as u32 })
	})
}

fn check_head(ctx: &mut Ctx, ast: Option<&ast::ClassHead>) -> ClassHead {
	let &ast::ClassHead(loc, ref head_data) = match ast {
		Some(h) => h,
		None => {
			if ctx.current_class.type_parameters.any() {
				todo!() // Error: static class can't have type parameters
			}
			return ClassHead::Static
		}
	};
	match head_data {
		&ast::ClassHeadData::Abstract(_) => {
			unused!(loc);
			todo!()
		}
		&ast::ClassHeadData::Slots(_) => {
			todo!()
		}
		&ast::ClassHeadData::Builtin => {
			ClassHead::Builtin
		}
	}
}

fn check_slot(ctx: &mut Ctx, slot_ast: &ast::Slot) -> SlotDeclaration {
	unused!(ctx, slot_ast);
	todo!()
}

fn check_super(ctx: &mut Ctx, super_ast: &ast::Super) -> Option<Super> {
	unused!(ctx, super_ast);
	todo!()
}

fn set_method_body(mut ctx: &mut Ctx, method: Ptr<MethodWithBody>, replacer: &TyReplacer, body: &ast::Expr) {
	let body = check_method_body(&mut ctx, &MethodOrImpl::Method(method.clone_ptr()), replacer, method.is_static, body);
	method.set_body(body)
}
