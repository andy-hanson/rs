use util::arena::Arena;
use util::arr::U8SliceOps;
use util::dict::MutDict;
use util::file_utils::IoError;
use util::path::Path;

use host::document_provider::file_system_document_provider;

use model::module::ModuleOrFail;

use super::builtins::BuiltinsOwn;

mod compiler;
mod module_resolver;
pub use self::compiler::compile;
pub use self::module_resolver::{full_path, EXTENSION};

pub struct CompiledProgram<'a> {
	//TODO:PERF should own this, not point to it
	pub builtins: &'a BuiltinsOwn<'a>,
	pub modules: MutDict<Path<'a>, ModuleOrFail<'a>>,
}

pub enum CompileResult<'a> {
	RootMissing,
	RootFound { program: CompiledProgram<'a>, root: ModuleOrFail<'a> },
}

pub fn compile_dir<'a>(dir: Path, arena: &'a Arena) -> Result<CompileResult<'a>, IoError> {
	compile(Path::EMPTY, &file_system_document_provider(dir), None, arena)
}

pub fn compile_file<'a>(path: Path<'a>, arena: &'a Arena) -> Result<CompileResult<'a>, IoError> {
	let file_name = path.last().unwrap().without_end_if_ends_with(EXTENSION); //TODO: magic constant
	let file_path = if file_name.equals_str("index") {
		Path::EMPTY
	} else {
		Path::of_slice(file_name)
	};
	compile(file_path, &file_system_document_provider(path.directory()), None, arena)
}
