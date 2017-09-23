use util::arena::Arena;
use util::iter::KnownLen;
use util::late::Late;
use util::list::{List, ListBuilder};
use util::loc::Loc;
use util::sym::Sym;
use util::up::Up;

use ast;

use model::builtins::BuiltinsOwn;
use model::class::{ClassDeclaration, InstClass, SlotDeclaration};
use model::diag::{Diag, Diagnostic};
use model::effect::Effect;
use model::method::{InstMethod, MethodOrImplOrAbstract};
use model::module::Module;
use model::ty::{PlainTy, Ty, TypeParameter};

use super::ast_utils::effect_to_effect;
use super::class_utils::{try_get_method_of_inst_class, MethodAndInferrer};
use super::inferrer::Inferrer;

pub struct Ctx<'builtins_ctx, 'model: 'builtins_ctx> {
	pub arena: &'model Arena,
	pub current_class: Up<'model, ClassDeclaration<'model>>,
	pub builtins: &'builtins_ctx BuiltinsOwn<'model>,
	imports: &'model [Up<'model, Module<'model>>],
	pub diags: ListBuilder<'model, Diagnostic<'model>>,
}
impl<'builtins_ctx, 'model: 'builtins_ctx> Ctx<'builtins_ctx, 'model> {
	pub fn new(
		current_class: Up<'model, ClassDeclaration<'model>>,
		builtins: &'builtins_ctx BuiltinsOwn<'model>,
		imports: &'model [Up<'model, Module<'model>>],
		arena: &'model Arena,
	) -> Self {
		Ctx { arena, current_class, builtins, imports, diags: ListBuilder::new(arena) }
	}

	pub fn finish(self) -> List<'model, Diagnostic<'model>> {
		self.diags.finish()
	}

	pub fn get_ty<'ast>(&mut self, ty_ast: &'ast ast::Ty<'ast>) -> Ty<'model> {
		//TODO:PERF -- version without type parameters?
		self.get_ty_or_ty_parameter(ty_ast, &[])
	}

	pub fn get_ty_or_ty_parameter<'ast>(
		&mut self,
		&ast::Ty { loc, effect, name, ty_args: ty_arg_asts }: &'ast ast::Ty<'ast>,
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
				if ty_arg_asts.is_empty() {
					return Ty::Param(Up(tp))
				} else {
					// Diagnostic: Can't provide type arguments to a type parameter
					unimplemented!()
				}
			}
		}

		//TODO: Use `?` operator when that works with Option
		let class = match self.access_class_declaration_or_add_diagnostic(loc, name) {
			Some(x) => x,
			None => return Ty::Bogus,
		};
		let ty_args = self.arena
			.map(ty_arg_asts, |ty_ast| Late::full(self.get_ty_or_ty_parameter(ty_ast, extra_ty_parameters)));
		Ty::Plain(PlainTy { effect: effect_to_effect(effect), inst_class: InstClass { class, ty_args } })
	}

	pub fn instantiate_class_from_ast<'ast>(
		&mut self,
		loc: Loc,
		name: Sym,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<InstClass<'model>> {
		self.access_class_declaration_or_add_diagnostic(loc, name)
			.and_then(|class| self.instantiate_class(class, ty_arg_asts))
	}

	pub fn instantiate_class<'ast>(
		&mut self,
		class: Up<'model, ClassDeclaration<'model>>,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<InstClass<'model>> {
		if ty_arg_asts.len() != class.type_parameters.len() {
			unimplemented!()
		} else {
			Some(InstClass { class, ty_args: self.get_ty_args(ty_arg_asts) })
		}
	}

	pub fn instantiate_method<'ast>(
		&mut self,
		loc: Loc,
		method_decl: MethodOrImplOrAbstract<'model>,
		ty_arg_asts: List<'ast, ast::Ty<'ast>>,
	) -> Option<InstMethod<'model>> {
		if ty_arg_asts.len() != method_decl.type_parameters().len() {
			self.add_diagnostic(
				loc,
				Diag::TypeArgumentCountMismatch {
					method: method_decl,
					n_arguments_provided: ty_arg_asts.len(),
				},
			);
			None
		} else {
			let ty_args = self.get_ty_args(ty_arg_asts);
			Some(InstMethod::new(method_decl, ty_args))
		}
	}

	fn get_ty_args<'ast>(&mut self, ty_arg_asts: List<'ast, ast::Ty<'ast>>) -> &'model [Late<Ty<'model>>] {
		self.arena
			.map(ty_arg_asts, |ty_ast| Late::full(self.get_ty(ty_ast)))
	}

	pub fn access_class_declaration_or_add_diagnostic(
		&mut self,
		loc: Loc,
		name: Sym,
	) -> Option<Up<'model, ClassDeclaration<'model>>> {
		let res = self.access_class_declaration(loc, name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::ClassNotFound(name))
		}
		res
	}

	fn access_class_declaration(
		&mut self,
		loc: Loc,
		name: Sym,
	) -> Option<Up<'model, ClassDeclaration<'model>>> {
		if name == self.current_class.name {
			return Some(self.current_class)
		}

		for i in self.imports {
			if i.name() == name {
				return Some(Up(&i.class))
			}
		}

		for b in *self.builtins.all_successes {
			if b.name() == name {
				return Some(Up(&b.class))
			}
		}

		self.add_diagnostic(loc, Diag::ClassNotFound(name));
		None
	}

	pub fn add_diagnostic(&mut self, loc: Loc, diag: Diag<'model>) {
		&mut self.diags <- Diagnostic { loc, diag };
	}

	pub fn get_own_slot(&mut self, loc: Loc, name: Sym) -> Option<Up<'model, SlotDeclaration<'model>>> {
		let current_class = self.current_class.clone_as_up();
		self.get_slot(loc, current_class, name)
	}

	pub fn get_slot(
		&mut self,
		loc: Loc,
		class: Up<'model, ClassDeclaration<'model>>,
		name: Sym,
	) -> Option<Up<'model, SlotDeclaration<'model>>> {
		let res = class.find_slot(name);
		if res.is_none() {
			self.add_diagnostic(loc, Diag::SlotNotFound(class, name));
		}
		res
	}

	pub fn get_own_method<'infer>(
		&mut self,
		loc: Loc,
		self_effect: Effect,
		name: Sym,
		arena: &'infer Arena,
	) -> Option<MethodAndInferrer<'infer, 'model>> {
		let current_inst_class = &self.current_inst_class();
		self.get_method_worker(loc, self_effect, current_inst_class, name, arena)
	}

	pub fn get_method_from_ty<'infer>(
		&mut self,
		loc: Loc,
		ty: &Ty<'model>,
		name: Sym,
		arena: &'infer Arena,
	) -> Option<MethodAndInferrer<'infer, 'model>> {
		match *ty {
			// Already should have made an error before creating the Bogus, so no need for another error.
			Ty::Bogus => None,
			Ty::Param(_) => unimplemented!(), //TODO:diagnostic
			Ty::Plain(PlainTy { effect: target_effect, ref inst_class }) =>
				self.get_method_worker(loc, target_effect, inst_class, name, arena),
		}
	}

	fn get_method_worker<'infer>(
		&mut self,
		loc: Loc,
		target_effect: Effect,
		inst_class: &InstClass<'model>,
		name: Sym,
		infer_arena: &'infer Arena,
	) -> Option<MethodAndInferrer<'infer, 'model>> {
		let res = try_get_method_of_inst_class(inst_class, name, infer_arena, self.arena);
		match res {
			Some(ref m) => {
				let method_decl = m.0;
				if !target_effect.contains(method_decl.self_effect()) {
					self.add_diagnostic(loc, Diag::IllegalSelfEffect { target_effect, method: method_decl })
					// Continue on anyway
				}
			}
			None => {
				let current_class = self.current_class;
				self.add_diagnostic(loc, Diag::MethodNotFound(current_class, name))
			}
		}
		res
	}

	//mv
	pub fn current_inst_class(&mut self) -> InstClass<'model> {
		//TODO: don't allocate this every time! Just store it on the ClassDeclaration!
		InstClass::generic_self_reference(self.current_class, self.arena)
	}

	//mv
	pub fn get_static_method<'infer>(
		&mut self,
		loc: Loc,
		class: Up<'model, ClassDeclaration<'model>>,
		method_name: Sym,
		infer_arena: &'infer Arena,
	) -> Option<MethodAndInferrer<'infer, 'model>> {
		match class.find_static_method(method_name) {
			Some(m) =>
				Some(MethodAndInferrer(
					MethodOrImplOrAbstract::Method(m),
					Inferrer::of_static_method(m, infer_arena, self.arena),
				)),
			None => {
				self.add_diagnostic(loc, Diag::StaticMethodNotFound(class, method_name));
				None
			}
		}
	}
}
