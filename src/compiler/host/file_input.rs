use std::fs::File;
use std::io::{ErrorKind, Read};
pub use std::io::Result;

use util::arr::Arr;
use util::path::Path;
use util::sym::Sym;

pub trait FileInput {
	fn root_name(&self) -> Sym;
	/*
	None for file not found.
	Result::Err for any other problems reading.
	*/
	fn read(&self, path: &Path) -> Result<Option<Arr<u8>>>;
}

pub struct NativeFileInput {
	root_dir: Path,
}
impl NativeFileInput {
	pub fn new(root_dir: Path) -> NativeFileInput {
		NativeFileInput { root_dir }
	}
}
impl FileInput for NativeFileInput {
	fn root_name(&self) -> Sym {
		Sym::from_slice(self.root_dir.last().unwrap().as_slice())
	}

	fn read(&self, path: &Path) -> Result<Option<Arr<u8>>> {
		let full_path = Path::resolve_with_root(&self.root_dir, path).to_string();
		match File::open(full_path) {
			Ok(mut f) => {
				let mut v = Vec::<u8>::new();
				f.read_to_end(&mut v)?;
				Ok(Some(Arr::from_vec(v)))
			}
			Err(e) => match e.kind() {
				ErrorKind::NotFound => Ok(None),
				_ => Err(e),
			},
		}
	}
}
