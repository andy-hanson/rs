use util::file_utils::{read_file, IoError};

use util::path::Path;
use util::sym::Sym;

pub trait FileInput {
	type Error;

	fn root_name(&self) -> Sym;
	/*
	None for file not found.
	Result::Err for any other problems reading.
	*/
	fn read(&self, path: &Path) -> Result<Option<Vec<u8>>, Self::Error>;
}

pub struct NativeFileInput {
	pub root_dir: Path,
}
impl NativeFileInput {
	pub fn new(root_dir: Path) -> Self {
		NativeFileInput { root_dir }
	}
}
impl FileInput for NativeFileInput {
	type Error = IoError;

	fn root_name(&self) -> Sym {
		Sym::from_slice(self.root_dir.last().unwrap())
	}

	fn read(&self, path: &Path) -> Result<Option<Vec<u8>>, Self::Error> {
		read_file(&Path::resolve_with_root(&self.root_dir, path))
	}
}
