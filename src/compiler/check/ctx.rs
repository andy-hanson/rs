use std::cell::{ RefCell, RefMut };

use util::arr::{ Arr, ArrBuilder };
use util::loc::Loc;
use util::ptr::{ Own, Ptr };
use util::sym::Sym;

use super::super::diag::{ Diagnostic, DiagnosticData };
use super::super::model::class::ClassDeclaration;
use super::super::model::module::Imported;
use super::super::model::ty::{ Ty, TypeParameter };
use super::super::parse::ast;

use super::class_utils::{ try_get_member_from_class_declaration, InstMember };

pub struct Ctx {
	pub current_class: Own<ClassDeclaration>,
	imports: Arr<Imported>,
	pub diags: RefCell<ArrBuilder<Diagnostic>>, //TODO: unsafecell
}
impl Ctx {
	pub fn new(current_class: Own<ClassDeclaration>, imports: Arr<Imported>) -> Ctx {
		Ctx { current_class, imports, diags: RefCell::new(ArrBuilder::new()) }
	}

	pub fn finish(self) -> (Own<ClassDeclaration>, Arr<Diagnostic>) {
		(self.current_class, self.diags.into_inner().finish())
	}

	pub fn get_ty(&self, ty_ast: &ast::Ty) -> Ty {
		//TODO:PERF -- versino without type parameters?
		self.get_ty_or_type_parameter(ty_ast, &Arr::empty())
	}

	pub fn get_ty_or_type_parameter(&self, ty_ast: &ast::Ty, type_parameters: &Arr<Own<TypeParameter>>) -> Ty {
		unused!(ty_ast, type_parameters);
		todo!()
	}

	pub fn access_class_declaration_or_add_diagnostic(&self, loc: Loc, name: Sym) -> Option<Ptr<ClassDeclaration>> {
		let res = self.access_class_declaration(name);
		if res.is_none() {
			self.add_diagnostic(loc, DiagnosticData::ClassNotFound(name))
		}
		res
	}

	fn access_class_declaration(&self, name: Sym) -> Option<Ptr<ClassDeclaration>> {
		if name == self.current_class.name {
			return Some(self.current_class.ptr())
		}

		for i in self.imports.iter() {
			if i.name() == name {
				return Some(i.imported_class())
			}
		}

		todo!() //TODO: handle builtins!
	}

	pub fn add_diagnostic(&self, loc: Loc, data: DiagnosticData) {
		self.borrow_diags().add(Diagnostic(loc, data))
	}

	//mv?
	pub fn get_own_member_or_add_diagnostic(&self, loc: Loc, name: Sym) -> Option<InstMember> {
		//TODO:neater
		let res = try_get_member_from_class_declaration(&self.current_class, name);
		if res.is_none() {
			self.add_diagnostic(loc, DiagnosticData::MemberNotFound(self.current_class.ptr(), name))
		}
		res
	}

	pub fn borrow_diags(&self) -> RefMut<ArrBuilder<Diagnostic>> {
		self.diags.borrow_mut()
	}
}
