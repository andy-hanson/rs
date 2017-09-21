use serde::{Serialize, Serializer};

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

use super::arena::{Arena, NoDrop};
use super::iter::KnownLen;
use super::show::{Show, Shower};
use super::u8_slice_ops::U8SliceOps;

#[derive(Copy, Clone)]
pub struct Path<'a>(&'a [u8]);
impl<'a> NoDrop for Path<'a> {}
impl<'a> Path<'a> {
	pub const EMPTY: Path<'static> = Path(&[]);

	pub fn of(slice: &'a [u8]) -> Self {
		assert_ne!(slice.first().cloned(), Some(b'/'));
		assert_ne!(slice.last().cloned(), Some(b'/'));
		Path(slice)
	}

	pub fn clone_path_to_arena(self, arena: &Arena) -> Path {
		Path::of(arena.copy_slice(self.0))
	}

	pub fn slice(self) -> &'a [u8] {
		self.0
	}

	pub fn resolve_with_root<'out>(root: Path, path: Path, arena: &'out Arena) -> Path<'out> {
		if path.is_empty() {
			root.clone_path_to_arena(arena)
		} else {
			let mut res = arena.exact_len_builder(root.0.len() + 1 + path.0.len());
			res.add_slice(root.0);
			&mut res <- b'/';
			res.add_slice(path.0);
			Path::of(res.finish())
		}
	}

	pub fn into_rel(self) -> RelPath<'a> {
		RelPath { n_parents: 0, rel_to_parent: self }
	}

	pub fn resolve<'out>(self, _: RelPath, _: &'out Arena) -> Path<'out> {
		unimplemented!()
	}

	pub fn child<'x, 'out, I: KnownLen<Item = &'x u8>>(
		self,
		child_name: I,
		arena: &'out Arena,
	) -> Path<'out> {
		assert!(is_path_part(child_name));
		if self.is_empty() {
			//TODO: use copy_slice if possible?
			let mut res = arena.exact_len_builder(child_name.len());
			for c in child_name {
				&mut res <- *c;
			}
			Path::of(res.finish())
		} else {
			let mut res = arena.exact_len_builder(self.0.len() + 1 + child_name.len());
			res.add_slice(self.0);
			&mut res <- b'/';
			for c in child_name {
				&mut res <- *c;
			}
			Path::of(res.finish())
		}
	}

	pub fn file_name(self) -> Option<&'a [u8]> {
		if self.0.is_empty() {
			None
		} else {
			Some(match self.get_last_slash_index() {
				Some(i) => &self.0[i + 1..self.0.len()],
				None => self.0,
			})
		}
	}

	pub fn without_extension(self, extension: &[u8]) -> Self {
		Path::of(self.0.without_end_if_ends_with(extension))
	}

	pub fn add_extension<'out>(self, extension: &[u8], arena: &'out Arena) -> Path<'out> {
		let mut b = arena.exact_len_builder(self.0.len() + extension.len());
		b.add_slice(self.0);
		b.add_slice(extension);
		Path::of(b.finish())
	}

	pub fn name_of_containing_directory(self) -> &'a [u8] {
		unimplemented!() //self.0.last().unwrap()
	}

	pub fn directory(self) -> Option<Self> {
		self.get_last_slash_index().map(|i| Path(&self.0[0..i]))
	}

	fn get_last_slash_index(&self) -> Option<usize> {
		// Count backwards to a '/'
		let mut res = None;
		for (i, &ch) in self.0.iter().enumerate().rev() {
			if ch == b'/' {
				// Rust won't let me break with value out of a for loop. :(
				res = Some(i);
				break
			}
		}
		res
	}

	fn is_empty(self) -> bool {
		self.0.is_empty()
	}
}
impl<'a> Show for Path<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.0)?;
		Ok(())
	}
}
impl<'a> Hash for Path<'a> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.0.hash(state);
	}
}
impl<'a> PartialEq for Path<'a> {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}
impl<'a> Eq for Path<'a> {}
impl<'a> Serialize for Path<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}
impl<'a> Borrow<[u8]> for Path<'a> {
	fn borrow(&self) -> &[u8] {
		self.0
	}
}

fn is_path_part<'x, I: IntoIterator<Item = &'x u8>>(s: I) -> bool {
	s.into_iter().all(|&ch| match ch {
		b'/' | b'\\' => false,
		_ => true,
	})
}

#[derive(Copy, Clone, Serialize)]
pub struct RelPath<'a> {
	pub n_parents: usize,
	pub rel_to_parent: Path<'a>,
}
impl<'a> RelPath<'a> {
	pub fn clone_path_to_arena<'out>(
		RelPath { n_parents, rel_to_parent }: RelPath,
		arena: &'out Arena,
	) -> RelPath<'out> {
		RelPath { n_parents, rel_to_parent: Path::clone_path_to_arena(rel_to_parent, arena) }
	}
	//pub fn clone_path(&self) -> RelPath {
 //	RelPath { n_parents: self.n_parents, rel_to_parent: self.rel_to_parent.clone_path() }
 //}
}
impl<'a> Show for RelPath<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		for _ in 0..self.n_parents {
			s.add("../")?;
		}
		s.add(self.rel_to_parent)?;
		Ok(())
	}
}
