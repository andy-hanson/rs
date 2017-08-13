use util::arr::Arr;

pub struct Path(pub Arr<Arr<u8>>);

pub struct RelPath {
	n_parents: u32,
	path: Path,
}
impl RelPath {
	pub fn of(n_parents: u32, path: Path) -> RelPath {
		RelPath { n_parents, path }
	}
}


