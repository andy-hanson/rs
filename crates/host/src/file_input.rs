use util::arena::Arena;
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
	fn read<'a>(&self, path: Path, arena: &'a Arena) -> Result<Option<&'a [u8]>, Self::Error>;
}

pub struct NativeFileInput<'a> {
	pub root_dir: Path<'a>,
}
impl<'a> NativeFileInput<'a> {
	pub fn new(root_dir: Path<'a>) -> Self {
		NativeFileInput { root_dir }
	}
}
impl<'a> FileInput for NativeFileInput<'a> {
	type Error = IoError;

	fn root_name(&self) -> Sym {
		Sym::from_slice(self.root_dir.last().unwrap())
	}

	fn read<'out>(&self, path: Path, arena: &'out Arena) -> Result<Option<&'out [u8]>, Self::Error> {
		read_file(Path::resolve_with_root(self.root_dir, path, arena), arena)
	}
}
