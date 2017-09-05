use util::arena::Arena;
use util::arr::{Arr, ArrBuilder};
use util::late::Late;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::host::document_info::make_readable;
use super::super::model::class::ClassDeclaration;
use super::super::model::module::{FailModule, Module, ModuleSourceEnum, OwnModuleOrFail};
use super::super::model::ty::{InstCls, Ty};

use super::check::check_module;
use super::parse::ast::Module as ModuleAst;
use super::parse::parse;

lazy_static! {
	static ref BUILTINS_FILES: Arr<(Sym, Arr<u8>)> = arr![
		(Sym::of("Void"), load_builtin_file(include_str!("../../../../builtins/Void.nz"))),
		(Sym::of("Bool"), load_builtin_file(include_str!("../../../../builtins/Bool.nz"))),
		(Sym::of("Nat"), load_builtin_file(include_str!("../../../../builtins/Nat.nz"))),
		(Sym::of("Int"), load_builtin_file(include_str!("../../../../builtins/Int.nz"))),
		(Sym::of("Float"), load_builtin_file(include_str!("../../../../builtins/Float.nz"))),
		(Sym::of("String"), load_builtin_file(include_str!("../../../../builtins/String.nz"))),
	];
}

fn load_builtin_file(text: &str) -> Arr<u8> {
	let v = text.as_bytes().to_owned();
	make_readable(v)
}

pub struct BuiltinsOwn {
	pub all: Arr<OwnModuleOrFail>,
	pub all_successes: Arr<Ptr<Module>>,
	pub void: Ty,
	pub bool: Ty,
}
impl BuiltinsOwn {
	#[allow(needless_lifetimes)] // Can't seem to write this without the lifetimes?
	pub fn as_ctx<'a>(&'a self) -> BuiltinsCtx<'a> {
		BuiltinsCtx { all_successes: &self.all_successes, void: Some(&self.void), bool: Some(&self.bool) }
	}
}

pub struct BuiltinsCtx<'a> {
	pub all_successes: &'a [Ptr<Module>],
	pub void: Option<&'a Ty>,
	pub bool: Option<&'a Ty>,
}

// Should only be called once, and after that re-used through old_program.
pub fn get_builtins() -> BuiltinsOwn {
	let mut builtins = ArrBuilder::<OwnModuleOrFail>::new();
	let mut all_successes = ArrBuilder::<Ptr<Module>>::new();
	let void = Late::<Ty>::new();
	let bool = Late::<Ty>::new();
	let sym_void = Sym::of("void");
	let sym_bool = Sym::of("bool");

	for &(name, ref text) in BUILTINS_FILES.iter() {
		let source = ModuleSourceEnum::Builtin { name, text };
		let arena = Arena::new();
		builtins.add(match parse(&arena, text) {
			Ok(ModuleAst { imports, class }) => {
				let module = Own::new(Module {
					source,
					imports: Arr::empty(),
					class: LateOwn::new(),
					diagnostics: LateOwn::new(),
				});
				assert!(!imports.any());
				{
					let cur_builtins = BuiltinsCtx {
						all_successes: &all_successes,
						void: void.try_get(),
						bool: bool.try_get(),
					};
					check_module(&module, &cur_builtins, &class, name);
				}
				if name == sym_void {
					void.init(primitive_ty(module.class.ptr()))
				} else if name == sym_bool {
					bool.init(primitive_ty(module.class.ptr()))
				}
				all_successes.add(module.ptr());
				OwnModuleOrFail::Module(module)
			}
			Err(e) =>
				OwnModuleOrFail::Fail(
					Own::new(FailModule { source, imports: Arr::empty(), diagnostics: Arr::_1(e) }),
				),
		});
	}

	BuiltinsOwn {
		all: builtins.finish(),
		all_successes: all_successes.finish(),
		void: void.into_option().unwrap_or(Ty::Bogus),
		bool: bool.into_option().unwrap_or(Ty::Bogus),
	}
}

fn primitive_ty(cls: Ptr<ClassDeclaration>) -> Ty {
	Ty::pure_ty(InstCls(cls, Arr::empty()))
}
