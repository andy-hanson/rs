use std::ops::{Add, Sub};

use util::arr::{Arr, ArrBuilder, SliceOps};

#[derive(Copy, Clone)]
pub struct Pos {
	pub index: u32,
}
impl Pos {
	pub const ZERO: Pos = Pos { index: 0 };

	pub fn decr(self) -> Pos {
		Pos { index: self.index - 1 }
	}
	pub fn incr(self) -> Pos {
		Pos { index: self.index + 1 }
	}
}
impl Add<u32> for Pos {
	type Output = Pos;

	fn add(self, rhs: u32) -> Pos {
		Pos { index: self.index + rhs }
	}
}
impl Sub<Pos> for Pos {
	type Output = u32;

	fn sub(self, rhs: Pos) -> u32 {
		self.index - rhs.index
	}
}

#[derive(Copy, Clone)]
pub struct Loc {
	pub start: Pos,
	pub end: Pos,
}
impl Loc {
	pub fn single_char(start: Pos) -> Loc {
		Loc { start, end: start.incr() }
	}

	pub fn slice_start_length(self, rel_start: u32, length: u32) -> Loc {
		let start = self.start + rel_start;
		Loc { start: start, end: start + length }
	}
}

pub struct LineAndColumn {
	pub line: u32,
	pub column: u32,
}

pub struct LineAndColumnLoc {
	pub start: LineAndColumn,
	pub end: LineAndColumn,
}
impl LineAndColumnLoc {
	pub fn single_line(line: u32, start_column: u32, end_column: u32) -> LineAndColumnLoc {
		LineAndColumnLoc {
			start: LineAndColumn { line, column: start_column },
			end: LineAndColumn { line, column: end_column },
		}
	}
}

pub struct LineAndColumnGetter {
	line_to_pos: Arr<u32>,
}
impl LineAndColumnGetter {
	pub fn new(text: &[u8]) -> LineAndColumnGetter {
		let mut line_to_pos = ArrBuilder::<u32>::new();
		line_to_pos.add(0); // line 0 as position 0
		for pos in text.range() {
			let ch = text[pos];
			if ch == b'\n' {
				line_to_pos.add((pos as u32) + 1)
			}
		}
		LineAndColumnGetter { line_to_pos: line_to_pos.finish() }
	}

	pub fn line_and_column_at_loc(&self, loc: Loc) -> LineAndColumnLoc {
		LineAndColumnLoc {
			start: self.line_and_column_at_pos(loc.start),
			end: self.line_and_column_at_pos(loc.end),
		}
	}

	fn line_and_column_at_pos(&self, pos: Pos) -> LineAndColumn {
		let pos_index = pos.index;
		let mut low_line: u32 = 0;
		let mut high_line = (self.line_to_pos.len() as u32) - 1;

		while low_line < high_line {
			let middle_line = mid(low_line, high_line);
			let middle_pos = self.line_to_pos[middle_line as usize];

			if middle_pos == pos_index {
				return LineAndColumn { line: middle_line, column: 0 }
			} else if middle_pos > pos_index {
				high_line = middle_line - 1
			} else {
				// middle_pos < pos_index
				low_line = middle_line + 1
			}
		}

		let line = low_line - 1;
		LineAndColumn { line, column: pos_index - self.line_to_pos[line as usize] }
	}
}

fn mid(a: u32, b: u32) -> u32 {
	(a + b) / 2
}
