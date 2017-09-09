use util::arena::Arena;
use util::path::Path;

use host::document_provider::DocumentProvider;

use model::document_info::DocumentInfo;

pub fn get_document_from_logical_path<'a, D: DocumentProvider<'a>>(
	document_provider: &D,
	logical_path: &Path,
	arena: &'a Arena,
) -> Result<GetDocumentResult<'a>, D::Error> {
	let regular = regular_path(logical_path, arena);
	if let Some(document) = document_provider.get_document(&regular, arena)? {
		return Ok(GetDocumentResult::Found { full_path: regular, is_index: false, document })
	};

	let index = index_path(logical_path, arena);
	if let Some(document) = document_provider.get_document(&index, arena)? {
		return Ok(GetDocumentResult::Found { full_path: index, is_index: true, document })
	};

	Ok(GetDocumentResult::NotFound)
}

pub enum GetDocumentResult<'a> {
	Found { full_path: Path<'a>, is_index: bool, document: DocumentInfo<'a> },
	NotFound,
}

pub fn full_path<'a>(logical_path: &Path, is_index: bool, arena: &'a Arena) -> Path<'a> {
	if is_index {
		index_path(logical_path, arena)
	} else {
		regular_path(logical_path, arena)
	}
}

fn regular_path<'a>(logical_path: &Path, arena: &'a Arena) -> Path<'a> {
	logical_path.add_extension(EXTENSION, arena)
}

fn index_path<'a>(logical_path: &Path, arena: &'a Arena) -> Path<'a> {
	logical_path.child(INDEX_NZ, arena)
}

pub const EXTENSION: &[u8] = b".nz";
const INDEX_NZ: &[u8] = b"index.nz";
