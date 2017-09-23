#![allow(dead_code)] // TODO
#![allow(unknown_lints)] // Clippy lints aren't known to rustc?
#![feature(collection_placement)]
#![feature(placement_in_syntax)]

extern crate ast;
extern crate check;
extern crate host;
extern crate model;
extern crate parse;
extern crate util;

mod builtins;
mod compiler;
mod module_resolver;

pub use self::compiler::compile;
pub use self::module_resolver::{full_path, EXTENSION};

use util::arena::Arena;
use util::file_utils::IoError;
use util::path::Path;
use util::u8_slice_ops::U8SliceOps;

use host::document_provider::file_system_document_provider;

use model::program::CompiledProgram;

pub enum CompileResult<'a> {
	RootMissing,
	RootFound(CompiledProgram<'a>),
}

pub fn compile_dir<'a>(dir: Path, arena: &'a Arena) -> Result<CompileResult<'a>, IoError> {
	compile(Path::EMPTY, &mut file_system_document_provider(dir), None, arena)
}

pub fn compile_file<'a>(path: Path<'a>, arena: &'a Arena) -> Result<CompileResult<'a>, IoError> {
	let file_name = path.file_name()
		.unwrap()
		.without_end_if_ends_with(EXTENSION);
	let file_path = if file_name == b"index" {
		Path::EMPTY
	} else {
		Path::of(file_name)
	};
	//TODO:unwrap may not be safe -- may be root path?
	compile(file_path, &mut file_system_document_provider(path.directory().unwrap()), None, arena)
}
