use util::dict::MutDict;
use util::path::Path;

use super::builtins::BuiltinsOwn;
use super::module::ModuleOrFail;

pub struct CompiledProgram<'a> {
	//TODO:PERF should own this, not point to it
	pub builtins: &'a BuiltinsOwn<'a>,
	pub modules: MutDict<Path<'a>, ModuleOrFail<'a>>,
	pub root: ModuleOrFail<'a>,
}
