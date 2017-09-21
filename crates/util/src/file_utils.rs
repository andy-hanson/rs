use std::fs::{read_dir as fs_read_dir, DirBuilder, File, ReadDir};
use std::io::{ErrorKind, Read, Write};

pub use std::io::{Error as IoError, Result as IoResult};

use super::arena::Arena;
use super::arith::u64_to_usize;
use super::dict::MutDict;
use super::path::Path;
use super::show::Show;

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

#[derive(Copy, Clone)]
pub enum ReadFileOptions {
	Plain,
	Trailing0,
}

// Returned paths are relative to the directory.
pub fn read_files_in_directory_recursive_if_exists<'a>(
	dir: Path,
	options: ReadFileOptions,
	arena: &'a Arena,
) -> IoResult<MutDict<Path<'a>, &'a [u8]>> {
	let mut res = MutDict::<Path, &'a [u8]>::new();
	let scratch = Arena::new();
	readdir_worker(dir, Path::EMPTY, &scratch, options, arena, &mut res).map(|()| res)
}
fn readdir_worker<'a>(
	base_dir: Path,
	cur_dir: Path,
	scratch: &Arena,
	options: ReadFileOptions,
	arena: &'a Arena,
	mut res: &mut MutDict<Path<'a>, &'a [u8]>,
) -> IoResult<()> {
	let full_dir = Path::resolve_with_root(base_dir, cur_dir, scratch);
	if let Some(rd) = read_dir(full_dir)? {
		for child_result in rd {
			let child = child_result?;
			let file_type = child.file_type()?;
			let child_path = cur_dir.child(child.file_name().into_string().unwrap().as_bytes(), arena);
			let child_full_path = Path::resolve_with_root(base_dir, child_path, scratch);
			if file_type.is_dir() {
				readdir_worker(base_dir, child_path, scratch, options, arena, &mut res)?
			} else if file_type.is_file() {
				res.add(child_path) <- do_read_file(child_full_path, options, arena)?;
			} else {
				unimplemented!()
			}
		}
	}
	Ok(())
}

pub fn read_file<'out>(
	path: Path,
	options: ReadFileOptions,
	arena: &'out Arena,
) -> IoResult<Option<&'out [u8]>> {
	match do_read_file(path, options, arena) {
		Ok(text) => Ok(Some(text)),
		Err(e) =>
			match e.kind() {
				ErrorKind::NotFound => Ok(None),
				_ => Err(e),
			},
	}
}

fn do_read_file<'out>(path: Path, options: ReadFileOptions, arena: &'out Arena) -> IoResult<&'out [u8]> {
	let mut file = File::open(path.to_string())?;
	let file_len = u64_to_usize(file.metadata()?.len());
	let trailing_0 = match options {
		ReadFileOptions::Plain => false,
		ReadFileOptions::Trailing0 => true,
	};
	// For safety, must ensure that we write to all of those uninitialized bytes!
	let buff = unsafe { arena.alloc_uninitialized(file_len + if trailing_0 { 1 } else { 0 }) };
	let mut idx = 0;
	loop {
		let n_bytes_read = file.read(&mut buff[idx..])?;
		assert_ne!(n_bytes_read, 0); // We must write to *all* of "buff", so can't stop early.
		idx += n_bytes_read;
		if idx == file_len {
			break
		} else {
			assert!(idx < buff.len());
		}
	}
	if trailing_0 {
		buff[file_len] = b'\0'
	}
	Ok(buff)
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
				_ => Err(e),
			}
		}
	}
}

fn create_directory(path: Path) -> IoResult<()> {
	DirBuilder::new().recursive(true).create(path.to_string())
}
