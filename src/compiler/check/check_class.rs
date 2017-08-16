use compiler::diag::{ Diagnostic };
use compiler::model::class_declaration::{ ClassDeclaration, ClassHead, SlotDeclaration, Super };
use compiler::model::method::{ MethodWithBody, Parameter };
use compiler::model::module::{ Imported };
use compiler::model::ty::{ Ty, TypeParameter, TypeParameterOrigin };

use compiler::parse::ast;

use util::arr::{ Arr, ArrBuilder };
use util::ptr::{ Own, Ptr };
use util::sym::Sym;

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
	let mut ctx = Ctx { current_class, imports, diags: ArrBuilder::new() };
	do_check(&mut ctx, ast);
	(ctx.current_class, ctx.diags.finish())
}

struct Ctx {
	current_class: Own<ClassDeclaration>,
	imports: Arr<Imported>,
	diags: ArrBuilder<Diagnostic>,
}
impl Ctx {
	fn get_ty_or_type_parameter(&self, ty_ast: &ast::Ty, type_parameters: &Arr<Own<TypeParameter>>) -> Ty {
		unused!(ty_ast, type_parameters);
		panic!()
	}
}

fn do_check(ctx: &mut Ctx, ast: ast::ClassDeclaration) {
	let ast::ClassDeclaration {
		loc: _, type_parameters: _, head: head_ast, supers: super_asts, methods: method_asts
	} = ast; // Already used type parameters

	let methods = method_asts.map(|m| check_method_initial(ctx, m));
	ctx.current_class.set_methods(methods);

	// Adds slots too
	let head = check_head(ctx, head_ast);
	ctx.current_class.set_head(head);

	let supers = super_asts.map_defined_probably_all(|super_ast| check_super(ctx, super_ast));
	ctx.current_class.set_supers(supers);

	// Now that all methods exist, fill in their bodies.
	for i in 0..method_asts.len() { //TODO: do_zip, or at least method_asts.range()
		let method_ast = &method_asts[i];
		let method = ctx.current_class.methods()[i].ptr();
		check_method_body(ctx, method, &TyReplacer::do_nothing(), &method_ast.body);

		//let current_class: &mut ClassDeclaration = Rc::get_mut(&mut ctx.current_class).unwrap();
		//let mut method = &mut current_class.methods()[i];
		//let body = check_method_body(ctx, &*method, &TyReplacer::do_nothing(), &method_ast.body);
		//Rc::get_mut(&mut method).unwrap().set_body(body)
	}

	//method_asts.do_zip(ctx.current_class.methods(), |method_ast, method| {
	//	method.set_body(check_method_body(ctx, method.borrow(), &TyReplacer::do_nothing(), &method_ast.body))
	//});
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

fn check_parameters(ctx: &mut Ctx, param_asts: &Arr<ast::Parameter>, type_parameters: &Arr<Own<TypeParameter>>) -> Arr<Parameter> {
	param_asts.map_with_index(|&ast::Parameter { loc, ty: ref ty_ast, name }, index| {
		for j in 0..index {
			if param_asts[j].name == name {
				panic!()
				//ctx.add_diagnostic(loc, )
			}
		}
		Parameter { loc, ty: ctx.get_ty_or_type_parameter(ty_ast, type_parameters), name, index: index as u32 }
	})
}

fn check_head(ctx: &mut Ctx, ast: Option<ast::Head>) -> ClassHead {
	unused!(ctx, ast);
	todo!()
}

fn check_slot(ctx: &mut Ctx, slot_ast: &ast::Slot) -> SlotDeclaration {
	unused!(ctx, slot_ast);
	todo!()
}

fn check_super(ctx: &mut Ctx, super_ast: &ast::Super) -> Option<Super> {
	unused!(ctx, super_ast);
	todo!()
}

fn check_method_body(ctx: &mut Ctx, method: Ptr<MethodWithBody>, replacer: &TyReplacer, body: &ast::Expr) {
	unused!(ctx, method, replacer, body); todo!()
}
