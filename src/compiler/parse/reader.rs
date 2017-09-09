use std::cell::Cell;
use std::slice::Iter;

use util::loc::Pos;

use super::super::super::host::document_info::assert_readable;

pub struct Reader<'text> {
	source: &'text [u8],
	iter: Iter<'text, u8>,
	peek: u8,
	pos_cell: Cell<Pos>,
}
impl<'text> Reader<'text> {
	pub fn new(source: &'text [u8]) -> Self {
		// Usually DocumentProvider does this, but builtins work differently.
		// Super important to assert this so we don't read past the end of `source`.
		assert_readable(source);
		let mut iter = source.iter();
		let peek = *iter.next().unwrap();
		Reader { source, iter, peek, pos_cell: Cell::new(Pos::ZERO) }
	}

	pub fn pos(&self) -> Pos {
		self.pos_cell.get()
	}

	pub fn peek(&self) -> u8 {
		self.peek
	}

	pub fn skip(&mut self) {
		let x = self.iter.next();
		self.peek = if let Some(ch) = x { *ch } else { b'\0' }
	}

	fn skip2(&mut self) {
		self.skip();
		self.skip();
	}

	pub fn read_char(&mut self) -> u8 {
		let res = self.peek;
		self.skip();
		res
	}

	pub fn slice_from(&self, start_pos: Pos) -> &'text [u8] {
		let a = start_pos.index as usize;
		let b = self.pos_cell.get().index as usize;
		&self.source[a..b]
	}
}
