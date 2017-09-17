use serde::{Serialize, Serializer};

use std::ops::{Add, Sub};

use super::arena::NoDrop;
use super::arith::{mid, u32_to_usize, usize_to_u32};
use super::show::{Show, Shower, serialize_as_show};

#[derive(Copy, Clone, Eq, PartialEq)]
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
	fn serialize<S : Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_u32(self.index)
	}
}
impl NoDrop for Pos {}

#[derive(Copy, Clone, Eq, PartialEq)]
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
impl Show for Loc {
	fn show<S : Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.start.index)?.add('-')?.add(self.end.index)?;
		Ok(())
	}
}
impl Serialize for Loc {
	fn serialize<S : Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serialize_as_show(*self, serializer)
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)] //TODO:shouldn't need Debug
pub struct LineAndColumn {
	pub line: u32,
	pub column: u32,
}
impl<'a> Show for &'a LineAndColumn {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.line)?.add(":")?.add(self.column)?;
		Ok(())
	}
}

#[derive(Copy, Clone)]
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
impl<'a> Show for &'a LineAndColumnLoc {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(&self.start)?.add("-")?.add(&self.end)?;
		Ok(())
	}
}

pub struct LineAndColumnGetter {
	line_to_pos: Box<[u32]>,
}
impl LineAndColumnGetter {
	pub fn new(text: &[u8]) -> Self {
		let mut line_to_pos = Vec::<u32>::new();
		line_to_pos.place_back() <- 0; // line 0 as position 0
		for (pos, &ch) in text.iter().enumerate() {
			if ch == b'\n' {
				line_to_pos.place_back() <- (pos as u32) + 1;
			}
		}
		LineAndColumnGetter { line_to_pos: line_to_pos.into_boxed_slice() }
	}

	pub fn loc_at_line_and_column(&self, lc_loc: LineAndColumnLoc) -> Loc {
		Loc { start: self.pos_at_line_and_column(lc_loc.start), end: self.pos_at_line_and_column(lc_loc.end) }
	}

	pub fn pos_at_line_and_column(&self, lc_pos: LineAndColumn) -> Pos {
		let line = u32_to_usize(lc_pos.line);
		let index = self.line_to_pos[line] + lc_pos.column;
		let next_line = line + 1;
		assert!(next_line == self.line_to_pos.len() || index < self.line_to_pos[next_line]);
		Pos { index }
	}

	pub fn line_and_column_at_loc(&self, loc: Loc) -> LineAndColumnLoc {
		LineAndColumnLoc {
			start: self.line_and_column_at_pos(loc.start),
			end: self.line_and_column_at_pos(loc.end),
		}
	}

	pub fn line_and_column_at_pos(&self, pos: Pos) -> LineAndColumn {
		let pos_index = pos.index;
		// Both low_line and high_line are inclusive,
		// meaning the range of possible answers is [low_line..high_line]
		// and we stop when this range has only one element.
		let mut low_line: usize = 0;
		// EXCLUSIVE, not a valid line.
		let mut high_line = self.line_to_pos.len();

		while low_line < high_line - 1 {
			// Pick a line in between them.
			let middle_line = mid(low_line, high_line);
			let middle_pos = self.line_to_pos[middle_line as usize];

			if pos_index == middle_pos {
				// Exact match (rare)
				return LineAndColumn { line: usize_to_u32(middle_line), column: 0 }
			} else if pos_index < middle_pos {
				// Picked a line too far.
				high_line = middle_line
			} else {
				// pos_index > middle_pos
				// This is a possible line; pos may be somewhere on this line.
				low_line = middle_line
			}
		}

		let line = low_line;
		let line_start = self.line_to_pos[line];
		assert!(pos_index >= line_start);
		if line < self.line_to_pos.len() - 1 {
			assert!(pos_index <= self.line_to_pos[line + 1]);
		}
		LineAndColumn { line: usize_to_u32(line), column: pos_index - line_start }
	}
}
