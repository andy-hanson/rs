use std::fmt::Write;
use std::io::{stderr, Stderr, Write as IoWrite};
use std::iter::Iterator;
use std::marker::Sized;

use super::arr::{SliceOps, U8SliceOps};

pub trait Show where Self : Sized
{
	fn show<S: Shower>(self, s: &mut S);

	fn to_string(self) -> String {
		StringMaker::stringify(self)
	}
}
impl Show for char {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_char(self)
	}
}
impl<'a> Show for &'a [u8] {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_bytes(self)
	}
}
impl<'a> Show for &'a str {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_bytes(self.as_bytes())
	}
}
impl Show for u8 {
	fn show<S : Shower>(self, s: &mut S) { s._add_u8(self) }
}
impl Show for u32 {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_u32(self)
	}
}
impl Show for usize {
	fn show<S : Shower>(self, s: &mut S) { s._add_usize(self) }
}
impl Show for i32 {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_int(self)
	}
}
impl Show for f64 {
	fn show<S: Shower>(self, s: &mut S) {
		s._add_float(self)
	}
}


pub trait Shower
where
	Self: Sized,
{
	fn nl(&mut self) -> &mut Self;
	fn _add_char(&mut self, char: char);
	fn _add_bytes(&mut self, bytes: &[u8]);
	fn _add_u8(&mut self, u: u8);
	fn _add_u32(&mut self, u: u32);
	fn _add_usize(&mut self, u: usize);
	fn _add_int(&mut self, i: i32);
	fn _add_float(&mut self, f: f64);

	fn add<T: Show>(&mut self, value: T) -> &mut Self {
		value.show(self);
		self
	}

	fn join<'a, T>(&mut self, arr: &'a [T]) -> &mut Self where &'a T : Show {
		if arr.any() {
			arr[0].show(self);
			for x in arr.iter().skip(1) {
				self.add(',');
				self.add(' ');
				x.show(self)
			}
		}
		self
	}

	//TODO:duplicate code (write join_iter)
	fn join_map<'a, T, U : Show, F : FnMut(&T) -> U>(&mut self, arr: &'a [T], mut f: F) -> &mut Self {
		if arr.any() {
			f(&arr[0]).show(self);
			for x in arr.iter().skip(1) {
				self.add(',');
				self.add(' ');
				f(x).show(self)
			}
		}
		self
	}
}

//mv
pub struct WriteShower<W: IoWrite>(W);
impl WriteShower<Stderr> {
	pub fn stderr() -> WriteShower<Stderr> {
		WriteShower::new(stderr())
	}

	pub fn write_stderr<S : Show>(shown: S) {
		Self::stderr().add(shown).nl();
	}
}
impl<W: IoWrite> WriteShower<W> {
	fn new(w: W) -> Self {
		WriteShower(w)
	}
}
impl<W: IoWrite> Shower for WriteShower<W> {
	fn nl(&mut self) -> &mut Self {
		self.0.write_all(b"\n").unwrap();
		self
	}

	fn _add_char(&mut self, char: char) {
		write!(&mut self.0, "{}", char).unwrap()
	}

	fn _add_bytes(&mut self, bytes: &[u8]) {
		self.0.write_all(bytes).unwrap()
	}

	fn _add_u8(&mut self, u: u8) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_u32(&mut self, u: u32) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_usize(&mut self, u: usize) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_int(&mut self, i: i32) {
		write!(&mut self.0, "{}", i).unwrap()
	}

	fn _add_float(&mut self, f: f64) {
		write!(&mut self.0, "{}", f).unwrap()
	}
}

pub struct StringMaker(String);
impl Shower for StringMaker {
	fn nl(&mut self) -> &mut Self {
		self.0.push('\n');
		self
	}

	fn _add_char(&mut self, ch: char) {
		self.0.push(ch)
	}

	fn _add_bytes(&mut self, bytes: &[u8]) {
		//TODO:PERF
		self.0.push_str(&bytes.clone_to_utf8_string())
	}

	fn _add_u8(&mut self, u: u8) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_u32(&mut self, u: u32) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_usize(&mut self, u: usize) {
		write!(&mut self.0, "{}", u).unwrap()
	}

	fn _add_int(&mut self, i: i32) {
		write!(&mut self.0, "{}", i).unwrap()
	}

	fn _add_float(&mut self, f: f64) {
		write!(&mut self.0, "{}", f).unwrap()
	}
}
impl StringMaker {
	pub fn new() -> StringMaker {
		StringMaker(String::new())
	}

	fn stringify<T: Show>(t: T) -> String {
		let mut s = StringMaker::new();
		t.show(&mut s);
		s.0
	}
}
