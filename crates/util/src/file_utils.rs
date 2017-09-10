use std::fs::{read_dir as fs_read_dir, File, ReadDir};
use std::io::{Error, ErrorKind, Result, Write};

use super::arena::Arena;
use super::dict::MutDict;
use super::path::Path;
use super::string_maker::Show;

pub type IoError = Error;
pub type IoResult<T> = Result<T>;

pub fn read_dir(dir: Path) -> Result<Option<ReadDir>> {
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
) -> Result<MutDict<Path<'a>, &'a [u8]>> {
	let mut res = MutDict::<Path, &'a [u8]>::new();
	readdir_worker(dir, arena, &mut res).map(|()| res)
}
pub fn readdir_worker<'a>(
	dir: Path,
	arena: &'a Arena,
	mut res: &mut MutDict<Path<'a>, &'a [u8]>,
) -> Result<()> {
	if let Some(rd) = read_dir(dir)? {
		for child_result in rd {
			let child = child_result?;
			let file_type = child.file_type()?;
			let child_path = dir.child(child.file_name().into_string().unwrap().as_bytes(), arena);
			if file_type.is_dir() {
				readdir_worker(child_path, arena, &mut res)?
			} else if file_type.is_file() {
				let text = arena.read_from_file(File::open(child_path.to_string())?)?;
				res.add(child_path, text)
			} else {
				unimplemented!()
			}
		}
	}
	Ok(())
}

pub fn read_file<'out>(path: Path, arena: &'out Arena) -> Result<Option<&'out [u8]>> {
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
pub fn write_file(path: Path, content: &[u8]) -> Result<()> {
	let mut file = File::create(path.to_string())?;
	file.write_all(content)
}

pub fn write_file_and_ensure_directory(path: Path, content: &[u8]) -> Result<()> {
	match File::create(path.to_string()) {
		Ok(mut file) => file.write_all(content),
		Err(e) => {
			let _ = e;
			//TODO: https://doc.rust-lang.org/std/fs/struct.DirBuilder.html may come in useful
			unimplemented!()
		}
	}
}
