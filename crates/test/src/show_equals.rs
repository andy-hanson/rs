use std::fmt::Write;

use util::show::{Color, Show, Shower};

pub fn show_equals<T: Show>(value: T, text: &[u8]) -> bool {
	let mut s = AssertionShower { expected: text, index: 0 };
	s.add(value).is_ok() && s.is_finished()
}

struct AssertionShower<'a> {
	expected: &'a [u8],
	//TODO: use iter
	index: usize,
}
impl<'a> AssertionShower<'a> {
	fn is_finished(&self) -> bool {
		self.index == self.expected.len()
	}

	fn next_expected(&mut self) -> Result<u8, ()> {
		if self.is_finished() {
			Err(())
		} else {
			let res = self.expected[self.index];
			self.index += 1;
			Ok(res)
		}
	}

	fn check_next_expected(&mut self, byte: u8) -> Result<(), ()> {
		if self.next_expected()? != byte {
			Err(())
		} else {
			Ok(())
		}
	}

	fn check_str(&mut self, str: &str) -> Result<(), ()> {
		//TODO:PERF
		for byte in str.as_bytes() {
			self.check_next_expected(*byte)?
		}
		Ok(())
	}
}
impl<'a> Shower for AssertionShower<'a> {
	type Error = (); // Thrown when not equal

	fn color<F: FnMut() -> Result<(), Self::Error>>(
		&mut self,
		_: Color,
		mut f: F,
	) -> Result<(), Self::Error> {
		f()
	}

	fn _add_char(&mut self, ch: char) -> Result<(), Self::Error> {
		let mut buff = [b'\0'; 1];
		ch.encode_utf8(&mut buff);
		self.check_next_expected(buff[0])
	}

	fn _add_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		//TODO:PERF
		for byte in bytes {
			self.check_next_expected(*byte)?
		}
		Ok(())
	}

	fn _add_u8(&mut self, u: u8) -> Result<(), Self::Error> {
		let mut str = String::new();
		write!(&mut str, "{}", u).unwrap();
		self.check_str(&str)
	}

	fn _add_u32(&mut self, u: u32) -> Result<(), Self::Error> {
		let mut str = String::new();
		write!(&mut str, "{}", u).unwrap();
		self.check_str(&str)
	}

	fn _add_usize(&mut self, u: usize) -> Result<(), Self::Error> {
		let mut str = String::new();
		write!(&mut str, "{}", u).unwrap();
		self.check_str(&str)
	}

	fn _add_int(&mut self, i: i32) -> Result<(), Self::Error> {
		let mut str = String::new();
		write!(&mut str, "{}", i).unwrap();
		self.check_str(&str)
	}


	fn _add_float(&mut self, f: f64) -> Result<(), Self::Error> {
		let mut str = String::new();
		write!(&mut str, "{}", f).unwrap();
		self.check_str(&str)
	}
}
