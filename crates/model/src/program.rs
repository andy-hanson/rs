use util::dict::MutDict;
use util::path::Path;

use super::builtins::BuiltinsOwn;
use super::module::ModuleOrFail;

pub struct CompiledProgram<'model> {
	//TODO:PERF should own this, not point to it
	pub builtins: &'model BuiltinsOwn<'model>,
	pub modules: MutDict<Path<'model>, ModuleOrFail<'model>>,
	pub root: ModuleOrFail<'model>,
}
