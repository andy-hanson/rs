use std::cmp::min;
use std::hash::{Hash, Hasher};

use util::arr::{Arr, ArrBuilder};
use util::string_maker::{Show, Shower, StringMaker};

pub struct Path(pub Arr<Arr<u8>>);
impl Path {
	pub fn empty() -> Path {
		Path(Arr::empty())
	}

	pub fn clone_path(&self) -> Path {
		Path(self.0.clone())
	}

	pub fn from_string(s: Arr<u8>) -> Path {
		Path(s.split(|ch| ch == b'/' || ch == b'\\'))
	}

	pub fn to_string(&self) -> String {
		StringMaker::stringify(self)
	}

	pub fn resolve_with_root(root: &Path, path: &Path) -> Path {
		Path(root.0.concat(&path.0))
	}

	pub fn into_rel(self) -> RelPath {
		RelPath { n_parents: 0, rel_to_parent: self }
	}

	pub fn child(&self, child_name: &Arr<u8>) -> Path {
		assert!(is_path_part(child_name));
		Path(self.0.rcons(child_name.clone()))
	}

	pub fn resolve_2(&self, rel1: &RelPath, rel2: &RelPath) {
		self.resolve(rel1).resolve(rel2);
	}

	pub fn resolve(&self, rel: &RelPath) -> Path {
		let n_parts_to_keep = self.0.len().checked_sub(rel.n_parents as usize).unwrap();
		// TODO:PERF this allocates an intermediate array, not really necessary
		let parent = self.0.copy_slice(0, n_parts_to_keep);
		Path(parent.concat(&rel.rel_to_parent.0))
	}

	pub fn rel_to(&self, other: &Path) -> RelPath {
		let min_length = min(self.0.len(), other.0.len());
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
		RelPath { n_parents, rel_to_parent }
	}

	pub fn last(&self) -> Option<&Arr<u8>> {
		self.0.last()
	}

	pub fn without_extension(&self, extension: &Arr<u8>) -> Path {
		let mut b = ArrBuilder::<Arr<u8>>::new();
		for part in self.0.slice_rtail() {
			b.add(part.clone())
		}
		let last_part = self.last().unwrap();
		assert!(last_part.ends_with(extension));
		b.add(last_part.copy_slice(0, last_part.len() - extension.len()));
		Path(b.finish())
	}

	pub fn add_extension(&self, extension: &Arr<u8>) -> Path {
		let parts = &self.0;
		assert!(parts.any());
		let mut b = ArrBuilder::<Arr<u8>>::new();
		for part in self.0.slice_rtail() {
			b.add(part.clone())
		}
		b.add(self.last().unwrap().concat(extension));
		Path(b.finish())
	}

	pub fn name_of_containing_directory(&self) -> &Arr<u8> {
		self.0.last().unwrap()
	}

	pub fn directory(&self) -> Path {
		Path(self.0.copy_rtail())
	}
}
impl Show for Path {
	fn show<S: Shower>(&self, s: &mut S) {
		s.join_arrs(&self.0);
	}
}
impl Hash for Path {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.0.hash(state);
	}
}
impl PartialEq for Path {
	fn eq(&self, other: &Path) -> bool {
		self.0
			.each_equals(&other.0, |a, b| a.each_equals(b, |x, y| x == y))
	}
}
impl Eq for Path {}

fn is_path_part(s: &Arr<u8>) -> bool {
	s.iter().all(|ch| match *ch {
		b'/' | b'\\' => false,
		_ => true,
	})
}

pub struct RelPath {
	pub n_parents: usize,
	pub rel_to_parent: Path,
}
