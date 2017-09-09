use serde::{Serialize, Serializer};

use std::ops::{Add, Sub};

use super::arena::NoDrop;
use super::string_maker::{Show, Shower};

#[derive(Copy, Clone)]
pub struct Pos {
	pub index: u32,
}
impl Pos {
	pub const ZERO: Self = Pos { index: 0 };

	pub fn decr(self) -> Self {
		Pos { index: self.index - 1 }
	}
	pub fn incr(self) -> Self {
		Pos { index: self.index + 1 }
	}
}
impl Add<u32> for Pos {
	type Output = Self;

	fn add(self, rhs: u32) -> Self {
		Pos { index: self.index + rhs }
	}
}
impl Sub<Pos> for Pos {
	type Output = u32;

	fn sub(self, rhs: Self) -> u32 {
		self.index - rhs.index
	}
}
impl Serialize for Pos {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		serializer.serialize_u32(self.index)
	}
}
impl NoDrop for Pos {}

#[derive(Copy, Clone, Serialize)]
pub struct Loc {
	pub start: Pos,
	pub end: Pos,
}
impl Loc {
	pub fn single_char(start: Pos) -> Self {
		Loc { start, end: start.incr() }
	}

	pub fn slice_start_length(self, rel_start: u32, length: u32) -> Self {
		let start = self.start + rel_start;
		Loc { start: start, end: start + length }
	}
}
impl NoDrop for Loc {}

pub struct LineAndColumn {
	pub line: u32,
	pub column: u32,
}
impl Show for LineAndColumn {
	fn show<S: Shower>(&self, s: &mut S) {
		s.add(&self.line).add(&":").add(&self.column);
	}
}

pub struct LineAndColumnLoc {
	pub start: LineAndColumn,
	pub end: LineAndColumn,
}
impl LineAndColumnLoc {
	pub fn single_line(line: u32, start_column: u32, end_column: u32) -> Self {
		LineAndColumnLoc {
			start: LineAndColumn { line, column: start_column },
			end: LineAndColumn { line, column: end_column },
		}
	}
}
impl Show for LineAndColumnLoc {
	fn show<S: Shower>(&self, s: &mut S) {
		s.add(&self.start).add(&"-").add(&self.end);
	}
}

pub struct LineAndColumnGetter {
	line_to_pos: Vec<u32>,
}
impl LineAndColumnGetter {
	pub fn new(text: &[u8]) -> Self {
		let mut line_to_pos = Vec::<u32>::new();
		line_to_pos.place_back() <- 0; // line 0 as position 0
		for (pos, ch) in text.iter().enumerate() {
			if *ch == b'\n' {
				line_to_pos.place_back() <- (pos as u32) + 1;
			}
		}
		LineAndColumnGetter { line_to_pos }
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
