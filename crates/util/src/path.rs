use serde::{Serialize, Serializer};

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

use super::arena::{Arena, DirectBuilder, NoDrop};
use super::string_maker::{Show, Shower};
use super::u8_slice_ops::U8SliceOps;

#[derive(Copy, Clone)]
pub struct Path<'a>(&'a [u8]);
impl<'a> NoDrop for Path<'a> {}
impl<'a> Path<'a> {
	pub const EMPTY: Path<'static> = Path(&[]);

	pub fn of_slice(slice: &'a [u8]) -> Self {
		assert_ne!(slice.first().cloned(), Some(b'/'));
		assert_ne!(slice.last().cloned(), Some(b'/'));
		Path(slice)
	}

	pub fn clone_path_to_arena(self, arena: &Arena) -> Path {
		Path::of_slice(arena.copy_slice(self.0))
	}

	pub fn slice(self) -> &'a [u8] {
		self.0
	}

	pub fn resolve_with_root<'out>(root: Path, path: Path, arena: &'out Arena) -> Path<'out> {
		let mut res = arena.direct_builder();
		res.add_slice(root.0);
		&mut res <- b'/';
		res.add_slice(path.0);
		Path::of_slice(res.finish())
	}

	pub fn into_rel(self) -> RelPath<'a> {
		RelPath { n_parents: 0, rel_to_parent: self }
	}

	pub fn child<'out>(self, child_name: &[u8], arena: &'out Arena) -> Path<'out> {
		assert!(is_path_part(child_name));
		if self.is_empty() {
			Path(arena.copy_slice(child_name))
		} else {
			let mut res: DirectBuilder<'out, u8> = arena.direct_builder();
			res.add_slice(self.0);
			&mut res <- b'/';
			res.add_slice(child_name);
			Path::of_slice(res.finish())
		}
	}

	pub fn resolve(self, rel: RelPath) -> Self {
		unused!(rel);
		//TODO: walk backwards until you've passed n_parents '/' characters, then concat with rel.rel_to_parent
		unimplemented!()
	}

	pub fn rel_to(self, other: Self) -> RelPath<'a> {
		unused!(other);
		/*let min_length = min(self.0.len(), other.0.len());
		let mut first_different_part = 0;
		loop {
			if first_different_part == min_length {
				break
			}

			if self.0[first_different_part] != other.0[first_different_part] {
				break
			}

			first_different_part += 1
		}

		let n_parents = self.0.len() - first_different_part - 1;
		let rel_to_parent = Path(other.0.copy_slice(first_different_part, other.0.len()));
		RelPath { n_parents, rel_to_parent }*/
		unimplemented!()
	}

	pub fn file_name(self) -> Option<&'a [u8]> {
		if self.0.is_empty() {
			None
		} else {
			// Count backwards to a '/'
			let mut res = self.0;
			for (i, ch) in self.0.iter().enumerate().rev() {
				if *ch == b'/' {
					// Rust won't let me break with value out of a for loop. :(
					res = &self.0[i..self.0.len()];
					break
				}
			}
			Some(res)
		}
	}

	pub fn without_extension(self, extension: &[u8]) -> Self {
		Path::of_slice(self.0.without_end_if_ends_with(extension))
	}

	pub fn add_extension<'out>(self, extension: &[u8], arena: &'out Arena) -> Path<'out> {
		let mut b = arena.direct_builder();
		b.add_slice(self.0);
		b.add_slice(extension);
		Path::of_slice(b.finish())
	}

	pub fn name_of_containing_directory(self) -> &'a [u8] {
		unimplemented!() //self.0.last().unwrap()
	}

	pub fn directory(self) -> Self {
		unimplemented!() //Path(self.0.copy_rtail())
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
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		serializer.serialize_str(&self.to_string())
	}
}
impl<'a> Borrow<[u8]> for Path<'a> {
	fn borrow(&self) -> &[u8] {
		self.0
	}
}

fn is_path_part(s: &[u8]) -> bool {
	s.iter().all(|ch| match *ch {
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
