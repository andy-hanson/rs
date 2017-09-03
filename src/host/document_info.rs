use util::arr::Arr;

pub struct DocumentInfo {
	pub text: Arr<u8>,
	pub version: u32,
}
impl DocumentInfo {
	pub fn of(text: Arr<u8>, version: u32) -> Self {
		DocumentInfo { text, version }
	}

	pub fn same_version_as(&self, other: &Self) -> bool {
		self.version == other.version
	}
}
