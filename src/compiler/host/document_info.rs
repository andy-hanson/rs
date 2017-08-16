use util::arr::Arr;

use compiler::parse::ast::Module;
use compiler::parse::{ parse, Result };

pub struct DocumentInfo {
	source: Arr<u8>,
	version: u32,
	parse_result: Result<Module>,
}
impl DocumentInfo {
	pub fn parse(source: Arr<u8>, version: u32) -> DocumentInfo {
		let parse_result = parse(&source);
		DocumentInfo { source, version, parse_result }
	}

	pub fn same_version_as(&self, other: &DocumentInfo) -> bool {
		self.version == other.version
	}
}
