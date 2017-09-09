use util::arena::{Arena, Up, List, ListBuilder};
use util::loc::Loc;
use util::sym::Sym;

use super::super::super::model::class::ClassDeclaration;
use super::super::super::model::diag::{Diag, Diagnostic};
use super::super::super::model::method::{InstMethod, MethodOrAbstract};
use super::super::super::model::module::Module;
use super::super::super::model::ty::{InstCls, Ty, TypeParameter};

use super::super::builtins::BuiltinsCtx;
use super::super::parse::ast;

use super::class_utils::{try_get_member_from_class_declaration, InstMember};

pub struct Ctx<'builtins_ctx, 'model : 'builtins_ctx> {
	pub arena: &'model Arena,
	pub current_class: &'model ClassDeclaration<'model>,
	builtins: &'builtins_ctx BuiltinsCtx<'model>,
	imports: &'model [Up<'model, Module<'model>>],
	pub diags: ListBuilder<'model, Diagnostic<'model>>,
}
impl<'builtins_ctx, 'model : 'builtins_ctx> Ctx<'builtins_ctx, 'model> {
	pub fn new(
		current_class: &'model ClassDeclaration<'model>,
		builtins: &'builtins_ctx BuiltinsCtx<'model>,
		imports: &'model [Up<'model, Module<'model>>],
		arena: &'model Arena,
	) -> Self {
		Ctx { arena, current_class, builtins, imports, diags: arena.list_builder() }
	}

	pub fn finish(self) -> List<'model, Diagnostic<'model>> {
		self.diags.finish()
	}

	pub fn void(&self) -> Ty<'model> {
		self.builtins.void.unwrap().clone()
	}

	pub fn bool(&self) -> Ty<'model> {
		self.builtins.bool.unwrap().clone()
	}

	pub fn get_ty(&self, ty_ast: &ast::Ty) -> Ty<'model> {
		//TODO:PERF -- version without type parameters?
		self.get_ty_or_type_parameter(ty_ast, &[])
	}

	pub fn get_ty_or_type_parameter(&self, ty_ast: &ast::Ty, type_parameters: &[TypeParameter<'model>]) -> Ty<'model> {
		unused!(ty_ast, type_parameters);
		unimplemented!()
	}

	pub fn instantiate_class_from_ast<'ast>(
		&self,
		loc: Loc,
		name: Sym,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
	) -> Option<InstCls<'model>> {
		self.access_class_declaration_or_add_diagnostic(loc, name)
			.and_then(|class| self.instantiate_class(class, ty_arg_asts))
	}

	pub fn instantiate_class<'ast>(
		&self,
		class: &'model ClassDeclaration<'model>,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
	) -> Option<InstCls<'model>> {
		if ty_arg_asts.len != class.type_parameters.len() {
			unimplemented!()
		} else {
			Some(InstCls(class, self.get_ty_args(ty_arg_asts)))
		}
	}

	pub fn instantiate_method<'ast>(
		&self,
		method_decl: &MethodOrAbstract<'model>,
		ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>,
	) -> Option<&'model InstMethod<'model>> {
		if ty_arg_asts.len != method_decl.type_parameters().len() {
			unimplemented!()
		} else {
			Some(self.arena <- InstMethod(method_decl.copy(), self.get_ty_args(ty_arg_asts)))
		}
	}

	fn get_ty_args<'ast>(&self, ty_arg_asts: &'ast List<'ast, ast::Ty<'ast>>) -> &'model [Ty<'model>] {
		ty_arg_asts.map(self.arena, |ty_ast| self.get_ty(ty_ast))
	}

	pub fn access_class_declaration_or_add_diagnostic(
		&self,
		loc: Loc,
		name: Sym,
	) -> Option<&'model ClassDeclaration<'model>> {
		let res = self.access_class_declaration(name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::ClassNotFound(name))
		}
		res
	}

	fn access_class_declaration(&self, name: Sym) -> Option<&'model ClassDeclaration<'model>> {
		if name == self.current_class.name {
			return Some(self.current_class)
		}

		for i in self.imports.iter() {
			if i.name() == name {
				return Some(&i.class)
			}
		}

		for b in self.builtins.all_successes.iter() {
			if b.name() == name {
				return Some(&b.class)
			}
		}

		unimplemented!() //Diagnostic: class not found
	}

	pub fn add_diagnostic(&self, loc: Loc, data: Diag<'model>) {
		self.diags.add() <- Diagnostic(loc, data);
	}

	//mv?
	pub fn get_own_member_or_add_diagnostic(&self, loc: Loc, name: Sym) -> Option<InstMember<'model>> {
		//TODO:neater
		let res = try_get_member_from_class_declaration(self.current_class, name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(Up(self.current_class), name))
		}
		res
	}
}
