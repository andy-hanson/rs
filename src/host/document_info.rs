use util::arr::Arr;

pub struct DocumentInfo {
	pub text: Arr<u8>,
	pub version: u32,
	// Make the constructor private, but not the field getters
	_private: (),
}
impl DocumentInfo {
	pub fn of(text: Vec<u8>, version: u32) -> Self {
		DocumentInfo { text: make_readable(text), version, _private: () }
	}

	pub fn same_version_as(&self, other: &Self) -> bool {
		self.version == other.version
	}
}

pub fn make_readable(mut text: Vec<u8>) -> Arr<u8> {
	assert_ne!(text.last().cloned(), Some(b'\0'));
	text.push(b'\0');
	Arr::from_vec(text)
}

pub fn assert_readable(text: &[u8]) {
	assert_eq!(text.last().cloned(), Some(b'\0'));
}
