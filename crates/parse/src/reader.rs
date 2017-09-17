use std::io::Error as IoError;
use std::slice::Iter;

use util::arith::u32_to_usize;
use util::loc::Pos;
use util::output_shower::OutputShower;
use util::show::Shower;

pub struct Reader<'text> {
	source: &'text [u8],
	iter: Iter<'text, u8>,
	peek: u8,
	pos: Pos,
}
impl<'text> Reader<'text> {
	pub fn new(source: &'text [u8]) -> Self {
		// Usually DocumentProvider does this, but builtins work differently.
		// Super important to assert this so we don't read past the end of `source`.
		assert_readable(source);
		let mut iter = source.iter();
		let peek = *iter.next().unwrap();
		Reader { source, iter, peek, pos: Pos::ZERO }
	}

	//TODO: cfg[debug]
	pub fn debug_show(&self) {
		let pos = u32_to_usize(self.pos.index);
		/*let mut nl_before: size = pos - 1; // If at the end of a line, show that line, not the next line.
		while nl_before > 0 && self.source[nl_before] != b'\n' {
			nl_before -= 1
		}*/
		let nl_before = {
			if pos == 0 {
				None
			} else {
				let mut i = pos - 1;
				loop {
					if self.source[i] == b'\n' {
						break Some(i)
					}
					if i == 0 {
						break None
					}
					i -= 1
				}
			}
		};
		let line_end = {
			let mut i = pos;
			loop {
				if self.source[i] == b'\n' {
					break i
				}
				if i == self.source.len() - 1 {
					break self.source.len()
				}
				i += 1
			}
		};

		let line_no = match nl_before {
			Some(n) => {
				let mut l = 2;
				for i in 0..n {
					if self.source[i] == b'\n' {
						l += 1
					}
				}
				l
			}
			None => 1,
		};

		self.debug_show_worker(line_no, pos, match nl_before { Some(n) => n + 1, None => 0 }, line_end)
			.unwrap();
	}
	fn debug_show_worker(
		&self,
		line_no: usize,
		pos: usize,
		line_start: usize,
		line_end: usize,
	) -> Result<(), IoError> {
		OutputShower::stderr().add("Pos ")?.add(pos)?.add(": Line ")?
			.add(line_no)?
			.add(" (")?.add(line_start)?.add("-")?.add(line_end)?.add("): ")?
			.add(&self.source[line_start..pos])?
			.add("|")?
			.add(&self.source[pos..line_end])?
			.nl()?;
		Ok(())
	}

	pub fn pos(&self) -> Pos {
		self.pos
	}

	pub fn peek(&self) -> u8 {
		self.peek
	}

	pub fn skip(&mut self) {
		// next() should always succeed, because source should end in a '\0' that ends lexing.
		self.peek = *self.iter.next().unwrap();
		self.pos = self.pos.incr()
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
		let a = u32_to_usize(start_pos.index);
		let b = u32_to_usize(self.pos.index);
		assert_ne!(a, b);
		&self.source[a..b]
	}
}

fn assert_readable(text: &[u8]) {
	assert_eq!(text.last().cloned(), Some(b'\0'));
}
