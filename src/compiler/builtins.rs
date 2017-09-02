use util::arr::{Arr, ArrBuilder};
use util::ptr::{Late, LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::model::class::ClassDeclaration;
use super::super::model::module::Module;
use super::super::model::ty::{InstCls, Ty};

use super::check::check_module;
use super::parse::ast::Module as ModuleAst;
use super::parse::parse;

lazy_static! {
	static ref BUILTINS_FILES: Arr<(Sym, Arr<u8>)> = arr![
		(Sym::of("Void"), Arr::copy_from_str(include_str!("../../builtins/Void.nz"))),
		(Sym::of("Bool"), Arr::copy_from_str(include_str!("../../builtins/Bool.nz"))),
		(Sym::of("Nat"), Arr::copy_from_str(include_str!("../../builtins/Nat.nz"))),
		(Sym::of("Int"), Arr::copy_from_str(include_str!("../../builtins/Int.nz"))),
		(Sym::of("Float"), Arr::copy_from_str(include_str!("../../builtins/Float.nz"))),
		(Sym::of("String"), Arr::copy_from_str(include_str!("../../builtins/String.nz"))),
	];
}

pub struct BuiltinsOwn {
	pub all: Arr<Own<Module>>,
	pub void: Ty,
	pub bool: Ty,
}
impl BuiltinsOwn {
	#[allow(needless_lifetimes)] // Can't seem to write this without the lifetimes?
	pub fn as_ctx<'a>(&'a self) -> BuiltinsCtx<'a> {
		BuiltinsCtx { all: self.all.as_slice(), void: Some(&self.void), bool: Some(&self.bool) }
	}
}

pub struct BuiltinsCtx<'a> {
	pub all: &'a [Own<Module>],
	pub void: Option<&'a Ty>,
	pub bool: Option<&'a Ty>,
}

// Should only be called once, and after that re-used through old_program.
pub fn get_builtins() -> BuiltinsOwn {
	let mut builtins = ArrBuilder::<Own<Module>>::new();
	let void = Late::<Ty>::new();
	let bool = Late::<Ty>::new();
	let sym_void = Sym::of("void");
	let sym_bool = Sym::of("bool");

	for &(name, ref source) in BUILTINS_FILES.iter() {
		let module = Own::new(Module {
			source: None,
			imports: Arr::empty(),
			class: LateOwn::new(),
			diagnostics: LateOwn::new(),
		});
		let ModuleAst { imports, class } = match parse(source.as_slice()) {
			Ok(module) => module,
			Err(e) => {
				// Parse error in a builtin
				unused!(e);
				unimplemented!()
			}
		};
		assert_eq!(imports.len(), 0);
		{
			let cur_builtins =
				BuiltinsCtx { all: builtins.as_slice(), void: void.try_get(), bool: bool.try_get() };
			check_module(&module, &cur_builtins, &class, name);
		}
		if name == sym_void {
			void.init(primitive_ty(module.class.ptr()))
		} else if name == sym_bool {
			bool.init(primitive_ty(module.class.ptr()))
		}
		builtins.add(module);
	}

	BuiltinsOwn { all: builtins.finish(), void: void.into_value(), bool: bool.into_value() }
}

fn primitive_ty(cls: Ptr<ClassDeclaration>) -> Ty {
	Ty::pure_ty(InstCls(cls, Arr::empty()))
}
