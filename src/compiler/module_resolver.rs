use util::arr::Arr;
use util::path::{Path, RelPath};

//pub fn get_document_from_logical_path() {...}

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
	logical_path.child(&INDEX_NZ)
}

lazy_static! {
static ref EXTENSION: Arr<u8> = Arr::copy_from_str(".nz");
static ref INDEX_NZ: Arr<u8> = Arr::copy_from_str("index.nz");
}
