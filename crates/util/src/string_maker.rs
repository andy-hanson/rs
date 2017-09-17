use std::fmt::Write;

use super::show::{Color, Show, Shower};
use super::u8_slice_ops::U8SliceOps;

#[derive(Debug)]
pub enum Void {}
pub struct StringMaker(String);
impl StringMaker {
	pub fn new() -> StringMaker {
		StringMaker(String::new())
	}

	pub fn stringify<T: Show>(t: T) -> String {
		let mut s = StringMaker::new();
		// Error is Void, so can't be an error.
		t.show(&mut s).unwrap();
		s.0
	}
}
impl Shower for StringMaker {
	type Error = Void;

	fn color<F : FnMut() -> Result<(), Self::Error>>(&mut self, _: Color, mut f: F) -> Result<(), Self::Error> {
		f()
	}

	fn _add_char(&mut self, ch: char) -> Result<(), Self::Error> {
		self.0.push(ch);
		Ok(())
	}

	fn _add_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		//TODO:PERF
		self.0.push_str(&bytes.clone_to_utf8_string());
		Ok(())
	}

	fn _add_u8(&mut self, u: u8) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u).unwrap();
		Ok(())
	}

	fn _add_u32(&mut self, u: u32) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u).unwrap();
		Ok(())
	}

	fn _add_usize(&mut self, u: usize) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u).unwrap();
		Ok(())
	}

	fn _add_int(&mut self, i: i32) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", i).unwrap();
		Ok(())
	}

	fn _add_float(&mut self, f: f64) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", f).unwrap();
		Ok(())
	}
}
