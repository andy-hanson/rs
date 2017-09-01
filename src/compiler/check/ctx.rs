use std::cell::{RefCell, RefMut};

use util::arr::{Arr, ArrBuilder};
use util::loc::Loc;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::compile::builtins::BuiltinsCtx;
use super::super::diag::{Diag, Diagnostic};
use super::super::model::class::ClassDeclaration;
use super::super::model::method::{InstMethod, MethodOrAbstract};
use super::super::model::module::Module;
use super::super::model::ty::{Ty, TypeParameter, InstCls};
use super::super::parse::ast;

use super::class_utils::{try_get_member_from_class_declaration, InstMember};

pub struct Ctx<'a> {
	pub current_class: &'a LateOwn<ClassDeclaration>,
	builtins: &'a BuiltinsCtx<'a>,
	imports: &'a Arr<Ptr<Module>>,
	pub diags: RefCell<ArrBuilder<Diagnostic>>, //TODO: unsafecell
}
impl<'a> Ctx<'a> {
	pub fn new(
		current_class: &'a LateOwn<ClassDeclaration>,
		builtins: &'a BuiltinsCtx<'a>,
		imports: &'a Arr<Ptr<Module>>,
	) -> Ctx<'a> {
		Ctx { current_class, builtins, imports, diags: RefCell::new(ArrBuilder::new()) }
	}

	pub fn finish(self) -> Arr<Diagnostic> {
		self.diags.into_inner().finish()
	}

	pub fn void(&self) -> Ty {
		self.builtins.void.unwrap().clone()
	}

	pub fn bool(&self) -> Ty {
		self.builtins.bool.unwrap().clone()
	}

	pub fn get_ty(&self, ty_ast: &ast::Ty) -> Ty {
		//TODO:PERF -- version without type parameters?
		self.get_ty_or_type_parameter(ty_ast, &Arr::empty())
	}

	pub fn get_ty_or_type_parameter(
		&self,
		ty_ast: &ast::Ty,
		type_parameters: &Arr<Own<TypeParameter>>,
	) -> Ty {
		unused!(ty_ast, type_parameters);
		todo!()
	}

	pub fn instantiate_class_from_ast(
		&self,
		loc: Loc,
		name: Sym,
		ty_arg_asts: &Arr<ast::Ty>,
	) -> Option<InstCls> {
		self.access_class_declaration_or_add_diagnostic(loc, name).and_then(|class|
			self.instantiate_class(class, ty_arg_asts))
	}

	pub fn instantiate_class(
		&self,
		class: Ptr<ClassDeclaration>,
		ty_arg_asts: &Arr<ast::Ty>
	) -> Option<InstCls> {
		if ty_arg_asts.len() != class.type_parameters.len() {
			todo!()
		} else {
			Some(InstCls(class.clone_ptr(), self.get_ty_args(ty_arg_asts)))
		}
	}

	pub fn instantiate_method(
		&self,
		method_decl: &MethodOrAbstract,
		ty_arg_asts: &Arr<ast::Ty>,
	) -> Option<InstMethod> {
		if ty_arg_asts.len() != method_decl.type_parameters().len() {
			todo!()
		} else {
			Some(InstMethod(method_decl.copy(), self.get_ty_args(ty_arg_asts)))
		}
	}

	fn get_ty_args(&self, ty_arg_asts: &Arr<ast::Ty>) -> Arr<Ty> {
		ty_arg_asts.map(|ty_ast| self.get_ty(ty_ast))
	}

	pub fn access_class_declaration_or_add_diagnostic(
		&self,
		loc: Loc,
		name: Sym,
	) -> Option<Ptr<ClassDeclaration>> {
		let res = self.access_class_declaration(name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::ClassNotFound(name))
		}
		res
	}

	fn access_class_declaration(&self, name: Sym) -> Option<Ptr<ClassDeclaration>> {
		if name == self.current_class.name {
			return Some(self.current_class.ptr())
		}

		for i in self.imports.iter() {
			if i.name() == name {
				return Some(i.class())
			}
		}

		for b in self.builtins.all.iter() {
			if b.name() == name {
				return Some(b.class())
			}
		}

		todo!() //Diagnostic: class not found
	}

	pub fn add_diagnostic(&self, loc: Loc, data: Diag) {
		self.borrow_diags().add(Diagnostic(loc, data))
	}

	//mv?
	pub fn get_own_member_or_add_diagnostic(&self, loc: Loc, name: Sym) -> Option<InstMember> {
		//TODO:neater
		let res = try_get_member_from_class_declaration(self.current_class, name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(self.current_class.ptr(), name))
		}
		res
	}

	pub fn borrow_diags(&self) -> RefMut<ArrBuilder<Diagnostic>> {
		self.diags.borrow_mut()
	}
}
