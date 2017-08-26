use util::arr::Arr;
use util::dict::MutDict;
use util::path::Path;
use util::ptr::Own;

use super::host::document_provider::DocumentProvider;
use super::host::file_input::Result;
use super::model::module::{Module, OwnModuleOrFail, PtrModuleOrFail};

mod builtins;
mod compiler;
mod module_resolver;
use self::compiler::compile;
pub use self::module_resolver::full_path;

pub struct CompiledProgram {
	builtins: Arr<Own<Module>>,
	modules: MutDict<Own<Path>, OwnModuleOrFail>,
}

pub enum CompileResult {
	RootMissing,
	RootFound(CompiledProgram, PtrModuleOrFail),
}

pub fn compile_dir(dir: Path) -> Result<CompileResult> {
	compile(Path::empty(), &DocumentProvider::file_system(dir), None)
}

pub fn compile_file(path: &Path) -> Result<CompileResult> {
	let file_name = path.last().unwrap().without_end_if_ends_with(".nz"); //TODO: magic constant
	let file_path = if file_name.equals_str("index") {
		Path::empty()
	} else {
		Path::from_string(file_name)
	};
	compile(file_path, &DocumentProvider::file_system(path.directory()), None)
}
