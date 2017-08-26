use util::arr::{Arr, ArrBuilder};
use util::ptr::{LateOwn, Own};
use util::sym::Sym;

use super::super::check::check_module;
use super::super::model::module::Module;
use super::super::parse::ast::Module as ModuleAst;
use super::super::parse::parse;

lazy_static! {
static ref BUILTINS: Arr<(Sym, Arr<u8>)> = arr![
(Sym::of("Void"), Arr::copy_from_str(include_str!("../../../builtins/Void.nz"))),
(Sym::of("Bool"), Arr::copy_from_str(include_str!("../../../builtins/Bool.nz"))),
(Sym::of("Nat"), Arr::copy_from_str(include_str!("../../../builtins/Nat.nz"))),
(Sym::of("Int"), Arr::copy_from_str(include_str!("../../../builtins/Int.nz"))),
(Sym::of("Float"), Arr::copy_from_str(include_str!("../../../builtins/Float.nz"))),
(Sym::of("String"), Arr::copy_from_str(include_str!("../../../builtins/String.nz")))
];
}

// This should only be called once, and the next time the old program's builtins will be used.
// So it doesn't matter whether we do something in here or in `lazy_static!`.
pub fn get_builtins() -> Arr<Own<Module>> {
	let mut builtins = ArrBuilder::<Own<Module>>::new();

	for &(name, ref source) in BUILTINS.iter() {
		let module = Own::new(Module {
			source: None,
			imports: Arr::empty(),
			class: LateOwn::new(),
			diagnostics: LateOwn::new(),
		});
		let ModuleAst { imports, class } = match parse(source) {
			Ok(module) => module,
			Err(e) => {
				// Parse error in a builtin
				unused!(e);
				todo!()
			}
		};
		assert_eq!(imports.len(), 0);
		check_module(&module, builtins.as_slice(), &class, name);
		builtins.add(module)
	}

	builtins.finish()
}
