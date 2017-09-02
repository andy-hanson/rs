use util::arr::Arr;
use util::path::{Path, RelPath};

use super::super::super::host::document_info::DocumentInfo;
use super::super::super::host::document_provider::DocumentProvider;
use super::super::super::host::file_input::Result;

pub fn get_document_from_logical_path(
	document_provider: &DocumentProvider,
	logical_path: &Path,
) -> Result<GetDocumentResult> {
	let regular = regular_path(logical_path);
	if let Some(document) = document_provider.get_document(&regular)? {
		return Ok(GetDocumentResult::Found { full_path: regular, is_index: false, document })
	};

	let index = index_path(logical_path);
	if let Some(document) = document_provider.get_document(&index)? {
		return Ok(GetDocumentResult::Found { full_path: index, is_index: true, document })
	};

	Ok(GetDocumentResult::NotFound)
}

pub enum GetDocumentResult {
	Found { full_path: Path, is_index: bool, document: DocumentInfo },
	NotFound,
}

pub fn attempted_paths(importer_path: &Path, imported_path: &RelPath) -> Arr<Path> {
	let logical_path = importer_path.resolve(imported_path);
	Arr::_2(regular_path(&logical_path), index_path(&logical_path))
}

pub fn full_path(logical_path: &Path, is_index: bool) -> Path {
	if is_index {
		index_path(logical_path)
	} else {
		regular_path(logical_path)
	}
}

fn regular_path(logical_path: &Path) -> Path {
	logical_path.add_extension(&EXTENSION)
}

fn index_path(logical_path: &Path) -> Path {
	logical_path.child(INDEX_NZ.clone())
}

lazy_static! {
	static ref EXTENSION: Arr<u8> = Arr::copy_from_str(".nz");
	static ref INDEX_NZ: Arr<u8> = Arr::copy_from_str("index.nz");
}
