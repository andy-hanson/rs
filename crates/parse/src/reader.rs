use std::cell::Cell;
use std::slice::Iter;

use util::arith::u32_to_usize;
use util::loc::Pos;
use util::string_maker::{Shower, WriteShower};

use model::document_info::assert_readable;

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

	//TODO: cfg[debug]
	pub fn debug_show(&self) {
		let pos = u32_to_usize(self.pos_cell.get().index);
		let mut nl_before = pos;
		while nl_before > 0 && self.source[nl_before] != b'\n' {
			nl_before -= 1
		}
		let mut nl_after = pos;
		while nl_after < self.source.len() && self.source[nl_after] != b'\n' {
			nl_after += 1
		}

		let mut line_no = 0;
		for i in 0..nl_before {
			if self.source[i] == b'\n' {
				line_no += 1
			}
		}

		let mut s = WriteShower::stderr();
		s.add("Line ").add(line_no).add(": ").add(&self.source[nl_before..pos]).add("|").add(&self.source[pos..nl_after]).nl();
	}

	pub fn pos(&self) -> Pos {
		self.pos_cell.get()
	}

	pub fn peek(&self) -> u8 {
		self.peek
	}

	pub fn skip(&mut self) {
		// next() should always succeed, because source should end in a '\0' that ends lexing.
		self.peek = *self.iter.next().unwrap();
		self.pos_cell.set(self.pos_cell.get() + 1)
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
		assert_ne!(a, b);
		&self.source[a..b]
	}
}
