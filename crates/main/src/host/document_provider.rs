use util::path::Path;
use util::sym::Sym;

use super::document_info::DocumentInfo;
use super::file_input::{FileInput, NativeFileInput};

pub trait DocumentProvider {
	type Error;

	fn root_name(&self) -> Sym;
	/*
	None for document not found.
	Result::Err for any other error.
	*/
	fn get_document(&self, path: &Path) -> Result<Option<DocumentInfo>, Self::Error>;
}

//TODO: how to make this an impl on FileLoadingDocumentProvider?
pub fn file_system_document_provider(root_dir: Path) -> FileLoadingDocumentProvider<NativeFileInput> {
	FileLoadingDocumentProvider { file_input: NativeFileInput::new(root_dir) }
}

pub struct FileLoadingDocumentProvider<FI: FileInput> {
	file_input: FI,
}
impl<FI: FileInput> DocumentProvider for FileLoadingDocumentProvider<FI> {
	type Error = FI::Error;

	fn root_name(&self) -> Sym {
		self.file_input.root_name()
	}

	fn get_document(&self, path: &Path) -> Result<Option<DocumentInfo>, Self::Error> {
		self.file_input.read(path).map(|result| {
			result.map(|source| DocumentInfo::of(source, /*version*/ 0))
		})
	}
}
