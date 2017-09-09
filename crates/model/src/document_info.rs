pub struct DocumentInfo<'a> {
	pub text: &'a [u8],
	pub version: u32,
	// Make the constructor private, but the field getters public
	_private: (),
}
impl<'a> DocumentInfo<'a> {
	pub fn of(text: &'a [u8], version: u32) -> Self {
		assert_readable(text);
		DocumentInfo { text, version, _private: () }
	}

	pub fn same_version_as(&self, other: &Self) -> bool {
		self.version == other.version
	}
}

pub fn make_readable(text: &mut Vec<u8>) {
	assert_ne!(text.last().cloned(), Some(b'\0'));
	text.push(b'\0')
}

pub fn assert_readable(text: &[u8]) {
	assert_eq!(text.last().cloned(), Some(b'\0'));
}
