use serde::{Serialize, Serializer};

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

use super::arena::{Arena, NoDrop, DirectBuilder};
use super::string_maker::{Show, Shower};
use super::u8_slice_ops::U8SliceOps;

#[derive(Copy, Clone)]
pub struct Path<'a>(&'a [u8]);
impl<'a> NoDrop for Path<'a> {}
impl<'a> Path<'a> {
	pub const EMPTY: Path<'static> = Path(&[]);

	pub fn clone_path_to_arena<'out>(&self, arena: &'out Arena) -> Path<'out> {
		Path(arena.clone_slice(self.0))
	}

	pub fn of_slice(slice: &'a [u8]) -> Self {
		Path(slice)
	}

	pub fn slice(&self) -> &'a [u8] {
		self.0
	}

	pub fn resolve_with_root<'out>(root: Path, path: Path, arena: &'out Arena) -> Path<'out> {
		let mut res = arena.direct_builder();
		res.add_slice(root.0);
		&mut res <- b'/';
		res.add_slice(path.0);
		Path(res.finish())
	}

	pub fn into_rel(self) -> RelPath<'a> {
		RelPath { n_parents: 0, rel_to_parent: self }
	}

	pub fn child<'out>(&self, child_name: &[u8], arena: &'out Arena) -> Path<'out> {
		assert!(is_path_part(child_name));
		let mut res: DirectBuilder<'out, u8> = arena.direct_builder();
		res.add_slice(self.0);
		&mut res <- b'/';
		res.add_slice(child_name);
		Path(res.finish())
	}

	pub fn resolve(&self, rel: &RelPath) -> Self {
		unused!(rel);
		//TODO: walk backwards until you've passed n_parents '/' characters, then concat with rel.rel_to_parent
		unimplemented!()
	}

	pub fn rel_to(&self, other: &Self) -> RelPath {
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

	pub fn last(&self) -> Option<&'a [u8]> {
		/*self.0.last().map(|a| {
			let slice: &[u8] = &*a;
			slice
		})*/
		unimplemented!()
	}

	pub fn without_extension(&self, extension: &[u8]) -> Self {
		Path(self.0.without_end_if_ends_with(extension))
	}

	pub fn add_extension<'out>(&self, extension: &[u8], arena: &'out Arena) -> Path<'out> {
		let mut b = arena.direct_builder();
		b.add_slice(self.0);
		b.add_slice(extension);
		Path(b.finish())
	}

	pub fn name_of_containing_directory(&self) -> &[u8] {
		unimplemented!() //self.0.last().unwrap()
	}

	pub fn directory(&self) -> Self {
		unimplemented!() //Path(self.0.copy_rtail())
	}
}
impl<'p, 'a> Show for &'p Path<'a> {
	fn show<S: Shower>(self, s: &mut S) {
		s.add(self.0);
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

#[derive(Serialize)]
pub struct RelPath<'a> {
	pub n_parents: usize,
	pub rel_to_parent: Path<'a>,
}
impl<'a> RelPath<'a> {
	pub fn clone_path_to_arena<'out>(
		&RelPath { n_parents, ref rel_to_parent }: &RelPath,
		arena: &'out Arena,
	) -> RelPath<'out> {
		RelPath { n_parents, rel_to_parent: Path::clone_path_to_arena(rel_to_parent, arena) }
	}
	//pub fn clone_path(&self) -> RelPath {
	//	RelPath { n_parents: self.n_parents, rel_to_parent: self.rel_to_parent.clone_path() }
	//}
}
impl<'p, 'a> Show for &'p RelPath<'a> {
	fn show<S: Shower>(self, s: &mut S) {
		for _ in 0..self.n_parents {
			s.add("../");
		}
		s.add(&self.rel_to_parent);
	}
}
