use util::arena::Arena;
use util::iter::KnownLen;
use util::list::{List, ListBuilder};
use util::loc::Loc;
use util::sym::Sym;
use util::up::Up;

use ast;

use model::class::ClassDeclaration;
use model::diag::{Diag, Diagnostic};
use model::method::{InstMethod, MethodOrAbstract};
use model::module::Module;
use model::ty::{InstCls, Ty, TypeParameter};

use super::super::builtins::BuiltinsOwn;

use super::ast_utils::effect_to_effect;
use super::expected::Expected;
use super::class_utils::{try_get_member_from_class_declaration, InstMember};

pub struct Ctx<'builtins_ctx, 'model: 'builtins_ctx> {
	pub arena: &'model Arena,
	pub current_class: &'model ClassDeclaration<'model>,
	// Just mutable so `expected_void` can be referenced mutably -- but it won't actually be mutated, only Expected::Infer is mutated.
	builtins: &'builtins_ctx BuiltinsOwn<'model>,
	imports: &'model [Up<'model, Module<'model>>],
	pub diags: ListBuilder<'model, Diagnostic<'model>>,
}
impl<'builtins_ctx, 'model: 'builtins_ctx> Ctx<'builtins_ctx, 'model> {
	pub fn new(
		current_class: &'model ClassDeclaration<'model>,
		builtins: &'builtins_ctx BuiltinsOwn<'model>,
		imports: &'model [Up<'model, Module<'model>>],
		arena: &'model Arena,
	) -> Self {
		Ctx { arena, current_class, builtins, imports, diags: ListBuilder::new(arena) }
	}

	pub fn finish(self) -> List<'model, Diagnostic<'model>> {
		self.diags.finish()
	}

	pub fn expected_void(&self) -> &'builtins_ctx mut Expected<'model> {
		self.builtins.expected_void.unsafe_get_mut()
	}

	pub fn expected_bool(&self) -> &'builtins_ctx mut Expected<'model> {
		self.builtins.expected_bool.unsafe_get_mut()
	}

	pub fn get_ty<'ast>(&mut self, ty_ast: &'ast ast::Ty<'ast>) -> Ty<'model> {
		//TODO:PERF -- version without type parameters?
		self.get_ty_or_ty_parameter(ty_ast, &[])
	}

	pub fn get_ty_or_ty_parameter<'ast>(
		&mut self,
		&ast::Ty { loc, effect, name, ty_args }: &'ast ast::Ty<'ast>,
		extra_ty_parameters: &'model [TypeParameter<'model>],
	) -> Ty<'model> {
		for tp in self.current_class
			.type_parameters
			.iter()
			.chain(extra_ty_parameters)
		{
			if name == tp.name {
				if effect != ast::Effect::Pure {
					//Diagnostic: Not allowed to specify an effect on a type parameter.
					unimplemented!()
				}
				if ty_args.is_empty() {
					return Ty::Param(tp)
				} else {
					// Diagnostic: Can't provide type arguments to a type parameter
					unimplemented!()
				}
			}
		}

		let cls = unwrap_or_return!(self.access_class_declaration_or_add_diagnostic(loc, name), Ty::Bogus);
		let args = self.arena
			.map(ty_args, |ty_ast| self.get_ty_or_ty_parameter(ty_ast, extra_ty_parameters));
		Ty::Plain(effect_to_effect(effect), InstCls(cls, args))
	}

	pub fn instantiate_class_from_ast<'ast>(
		&mut self,
		loc: Loc,
		name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<InstCls<'model>> {
		self.access_class_declaration_or_add_diagnostic(loc, name)
			.and_then(|class| self.instantiate_class(class, ty_arg_asts))
	}

	pub fn instantiate_class<'ast>(
		&mut self,
		class: &'model ClassDeclaration<'model>,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<InstCls<'model>> {
		if ty_arg_asts.len() != class.type_parameters.len() {
			unimplemented!()
		} else {
			Some(InstCls(class, self.get_ty_args(ty_arg_asts)))
		}
	}

	pub fn instantiate_method<'ast>(
		&mut self,
		method_decl: MethodOrAbstract<'model>,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<&'model InstMethod<'model>> {
		if ty_arg_asts.len() != method_decl.type_parameters().len() {
			unimplemented!()
		} else {
			Some(self.arena <- InstMethod(method_decl, self.get_ty_args(ty_arg_asts)))
		}
	}

	fn get_ty_args<'ast>(&mut self, ty_arg_asts: List<'ast, ast::Ty<'ast>>) -> &'model [Ty<'model>] {
		self.arena.map(ty_arg_asts, |ty_ast| self.get_ty(ty_ast))
	}

	pub fn access_class_declaration_or_add_diagnostic(
		&mut self,
		loc: Loc,
		name: Sym,
	) -> Option<&'model ClassDeclaration<'model>> {
		let res = self.access_class_declaration(loc, name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::ClassNotFound(name))
		}
		res
	}

	fn access_class_declaration(&mut self, loc: Loc, name: Sym) -> Option<&'model ClassDeclaration<'model>> {
		if name == self.current_class.name {
			return Some(self.current_class)
		}

		for i in self.imports {
			if i.name() == name {
				return Some(&i.class)
			}
		}

		for b in *self.builtins.all_successes {
			if b.name() == name {
				return Some(&b.class)
			}
		}

		self.add_diagnostic(loc, Diag::ClassNotFound(name));
		None
	}

	pub fn add_diagnostic(&mut self, loc: Loc, diag: Diag<'model>) {
		&mut self.diags <- Diagnostic { loc, diag };
	}

	//mv?
	pub fn get_own_member_or_add_diagnostic(&mut self, loc: Loc, name: Sym) -> Option<InstMember<'model>> {
		//TODO:neater
		let res = try_get_member_from_class_declaration(self.current_class, name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::MemberNotFound(Up(self.current_class), name))
		}
		res
	}
}
