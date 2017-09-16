use util::arena::NoDrop;
use util::late::Late;
use util::up::Up;

use super::module::{Module, ModuleOrFail};
use super::ty::Ty;

//TODO: less Late
pub struct BuiltinsOwn<'model> {
	pub all: Late<&'model [ModuleOrFail<'model>]>,
	pub all_successes: Late<&'model [Up<'model, Module<'model>>]>,
	pub void: Late<Ty<'model>>,
	pub bool: Late<Ty<'model>>,
}

impl<'model> NoDrop for BuiltinsOwn<'model> {}
