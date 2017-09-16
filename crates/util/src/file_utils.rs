use std::fs::{read_dir as fs_read_dir, File, ReadDir, DirBuilder};
use std::io::{ErrorKind, Write};

pub use std::io::{Error as IoError, Result as IoResult};

use super::arena::Arena;
use super::dict::MutDict;
use super::path::Path;
use super::string_maker::Show;

pub fn read_dir(dir: Path) -> IoResult<Option<ReadDir>> {
	match fs_read_dir(dir.to_string()) {
		Ok(d) => Ok(Some(d)),
		Err(e) =>
			match e.kind() {
				ErrorKind::NotFound => Ok(None),
				_ => Err(e),
			},
	}
}

pub fn read_files_in_directory_recursive_if_exists<'a>(
	dir: Path,
	arena: &'a Arena,
) -> IoResult<MutDict<Path<'a>, &'a [u8]>> {
	let mut res = MutDict::<Path, &'a [u8]>::new();
	readdir_worker(dir, arena, &mut res).map(|()| res)
}
pub fn readdir_worker<'a>(
	dir: Path,
	arena: &'a Arena,
	mut res: &mut MutDict<Path<'a>, &'a [u8]>,
) -> IoResult<()> {
	if let Some(rd) = read_dir(dir)? {
		for child_result in rd {
			let child = child_result?;
			let file_type = child.file_type()?;
			let child_path = dir.child(child.file_name().into_string().unwrap().as_bytes(), arena);
			if file_type.is_dir() {
				readdir_worker(child_path, arena, &mut res)?
			} else if file_type.is_file() {
				res.add(child_path) <- arena.read_from_file(File::open(child_path.to_string())?)?;
			} else {
				unimplemented!()
			}
		}
	}
	Ok(())
}

pub fn read_file<'out>(path: Path, arena: &'out Arena) -> IoResult<Option<&'out [u8]>> {
	match File::open(path.to_string()) {
		Ok(/*mut*/ f) => arena.read_from_file(f).map(Some),
		Err(e) =>
			match e.kind() {
				ErrorKind::NotFound => Ok(None),
				_ => Err(e),
			},
	}
}

// Creates the file if it doesn't already exists.
pub fn write_file(path: Path, content: &[u8]) -> IoResult<()> {
	let mut file = File::create(path.to_string())?;
	file.write_all(content)
}

pub fn write_file_and_ensure_directory(path: Path, content: &[u8]) -> IoResult<()> {
	match File::create(path.to_string()) {
		Ok(mut file) => file.write_all(content),
		Err(e) => {
			match e.kind() {
				ErrorKind::NotFound => {
					// Path must have a directory, else we should have been able to place the file into the root directory.
					create_directory(path.directory().unwrap())?;
					write_file(path, content)
				}
				_ => Err(e)
			}
		}
	}
}

fn create_directory(path: Path) -> IoResult<()> {
	DirBuilder::new().recursive(true).create(path.to_string())
}
