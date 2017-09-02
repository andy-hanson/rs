use util::arr::Arr;

pub struct DocumentInfo {
	pub source: Arr<u8>,
	pub version: u32,
}
impl DocumentInfo {
	pub fn of(source: Arr<u8>, version: u32) -> DocumentInfo {
		DocumentInfo { source, version }
	}

	pub fn same_version_as(&self, other: &DocumentInfo) -> bool {
		self.version == other.version
	}
}
