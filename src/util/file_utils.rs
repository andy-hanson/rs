use std::fs::File;
use std::io::{Error, ErrorKind, Read, Result, Write};

use super::dict::MutDict;
use super::path::Path;
use super::string_maker::Show;

pub type IoError = Error;
pub type IoResult<T> = Result<T>;

//mv
pub fn read_files_in_directory_recursive_if_exists(dir: &Path) -> MutDict<Path, Vec<u8>> {
	let _ = dir;
	unimplemented!()
}

pub fn read_file(path: &Path) -> Result<Option<Vec<u8>>> {
	match File::open(path.to_string()) {
		Ok(mut f) => {
			let mut v = Vec::<u8>::new();
			f.read_to_end(&mut v)?;
			Ok(Some(v))
		}
		Err(e) =>
			match e.kind() {
				ErrorKind::NotFound => Ok(None),
				_ => Err(e),
			},
	}
}

// Creates the file if it doesn't already exists.
pub fn write_file(path: &Path, content: &[u8]) -> Result<()> {
	let mut file = File::create(path.to_string())?;
	file.write_all(content)
}

pub fn write_file_and_ensure_directory(path: &Path, content: &[u8]) -> Result<()> {
	match File::create(path.to_string()) {
		Ok(mut file) => file.write_all(content),
		Err(e) => {
			let _ = e;
			//TODO: https://doc.rust-lang.org/std/fs/struct.DirBuilder.html may come in useful
			unimplemented!()
		}
	}
}
