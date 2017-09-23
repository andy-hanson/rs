use serde::Serializer;

pub use termcolor::Color;

use super::string_maker::StringMaker;

pub trait Show
where
	Self: Sized,
{
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error>;

	fn to_string(self) -> String {
		StringMaker::stringify(self)
	}
}
impl Show for char {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_char(self)
	}
}
impl<'a> Show for &'a [u8] {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_bytes(self)
	}
}
impl<'a> Show for &'a str {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_bytes(self.as_bytes())
	}
}
impl Show for u8 {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_u8(self)
	}
}
impl Show for u32 {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_u32(self)
	}
}
impl Show for usize {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_usize(self)
	}
}
impl Show for i32 {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_int(self)
	}
}
impl Show for f64 {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s._add_float(self)
	}
}

pub trait Shower
where
	Self: Sized,
{
	type Error;

	// Does nothing (except call the callback) if this is not a colored output.
	fn color<F: FnMut() -> Result<(), Self::Error>>(&mut self, c: Color, f: F) -> Result<(), Self::Error>;

	fn nl(&mut self) -> Result<&mut Self, Self::Error> {
		self._add_char('\n')?;
		Ok(self)
	}

	fn _add_char(&mut self, char: char) -> Result<(), Self::Error>;
	fn _add_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
	fn _add_u8(&mut self, u: u8) -> Result<(), Self::Error>;
	fn _add_u32(&mut self, u: u32) -> Result<(), Self::Error>;
	fn _add_usize(&mut self, u: usize) -> Result<(), Self::Error>;
	fn _add_int(&mut self, i: i32) -> Result<(), Self::Error>;
	fn _add_float(&mut self, f: f64) -> Result<(), Self::Error>;

	fn add<T: Show>(&mut self, value: T) -> Result<&mut Self, Self::Error> {
		value.show(self)?;
		Ok(self)
	}

	fn join<'a, T: Show, I: IntoIterator<Item = T>>(&mut self, i: I) -> Result<&mut Self, Self::Error> {
		let mut iter = i.into_iter();
		if let Some(x) = iter.next() {
			x.show(self)?;
			while let Some(x) = iter.next() {
				self.add(',')?;
				self.add(' ')?;
				x.show(self)?;
			}
		}
		Ok(self)
	}
}

pub fn serialize_as_show<T: Show, S: Serializer>(t: T, serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&StringMaker::stringify(t))
}
