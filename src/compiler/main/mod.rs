use util::arr::U8SliceOps;
use util::dict::MutDict;
use util::file_utils::IoError;
use util::path::Path;
use util::ptr::Own;

use super::super::host::document_provider::file_system_document_provider;
use super::super::model::module::{OwnModuleOrFail, PtrModuleOrFail};

use super::builtins::BuiltinsOwn;

mod compiler;
mod module_resolver;
pub use self::compiler::compile;
pub use self::module_resolver::{full_path, EXTENSION};

pub struct CompiledProgram {
	builtins: BuiltinsOwn,
	pub modules: MutDict<Own<Path>, OwnModuleOrFail>,
}

pub enum CompileResult {
	RootMissing,
	RootFound(CompiledProgram, PtrModuleOrFail),
}

pub fn compile_dir(dir: Path) -> Result<CompileResult, IoError> {
	compile(Path::empty(), &file_system_document_provider(dir), None)
}

pub fn compile_file(path: &Path) -> Result<CompileResult, IoError> {
	let file_name = path.last().unwrap().without_end_if_ends_with(&EXTENSION); //TODO: magic constant
	let file_path = if file_name.equals_str("index") {
		Path::empty()
	} else {
		Path::from_string(file_name)
	};
	compile(file_path, &file_system_document_provider(path.directory()), None)
}
