use std::fmt::Write;
use std::iter::Iterator;
use std::marker::Sized;

use util::arr::{Arr, SliceOps, U8SliceOps};

pub trait Show
where
	Self: Sized,
{
	fn show<S: Shower>(&self, s: &mut S);

	fn to_string(&self) -> String {
		StringMaker::stringify(self)
	}
}

pub trait Shower
where
	Self: Sized,
{
	fn nl(&mut self) -> &mut Self;
	fn add_char(&mut self, char: char) -> &mut Self;
	fn add_bytes(&mut self, bytes: &[u8]) -> &mut Self;
	fn add_str(&mut self, str: &'static str) -> &mut Self;
	fn add_uint(&mut self, u: u32) -> &mut Self;
	fn add_int(&mut self, i: i32) -> &mut Self;
	fn add_float(&mut self, f: f64) -> &mut Self;

	fn join<T: Show>(&mut self, arr: &[T]) -> &mut Self {
		if arr.any() {
			arr[0].show(self);
			for x in arr.iter().skip(1) {
				self.add_char(',');
				self.add_char(' ');
				x.show(self)
			}
		}
		self
	}

	fn join_arrs(&mut self, arr: &[Arr<u8>]) -> &mut Self {
		if arr.any() {
			self.add_bytes(&arr[0]);
			for x in arr.iter().skip(1) {
				self.add_char(',');
				self.add_char(' ');
				self.add_bytes(x);
			}
		}
		self
	}
}

pub struct StringMaker(String);
impl Shower for StringMaker {
	fn nl(&mut self) -> &mut Self {
		self.0.push('\n');
		self
	}

	fn add_char(&mut self, ch: char) -> &mut Self {
		self.0.push(ch);
		self
	}

	fn add_bytes(&mut self, bytes: &[u8]) -> &mut Self {
		//TODO:PERF
		self.0.push_str(&bytes.clone_to_utf8_string());
		self
	}

	fn add_str(&mut self, s: &'static str) -> &mut Self {
		self.0.push_str(s);
		self
	}

	fn add_uint(&mut self, u: u32) -> &mut Self {
		write!(&mut self.0, "{}", u).unwrap();
		self
	}

	fn add_int(&mut self, i: i32) -> &mut Self {
		write!(&mut self.0, "{}", i).unwrap();
		self
	}

	fn add_float(&mut self, f: f64) -> &mut Self {
		write!(&mut self.0, "{}", f).unwrap();
		self
	}
}
impl StringMaker {
	pub fn new() -> StringMaker {
		StringMaker(String::new())
	}

	fn stringify<T: Show>(t: &T) -> String {
		let mut s = StringMaker::new();
		t.show(&mut s);
		s.0
	}
}
