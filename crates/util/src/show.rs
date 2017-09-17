use serde::Serializer;

use std::io::{stderr, Error as IoError, Stderr, Write as IoWrite};

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

	fn join<'a, T>(&mut self, arr: &'a [T]) -> Result<&mut Self, Self::Error>
		where
			&'a T: Show,
	{
		if !arr.is_empty() {
			arr[0].show(self)?;
			for x in arr.iter().skip(1) {
				self.add(',')?;
				self.add(' ')?;
				x.show(self)?;
			}
		}
		Ok(self)
	}

	//TODO:duplicate code (write join_iter)
	fn join_map<'a, T, U: Show, F: FnMut(&T) -> U>(
		&mut self,
		arr: &'a [T],
		mut f: F,
	) -> Result<&mut Self, Self::Error> {
		if !arr.is_empty() {
			f(&arr[0]).show(self)?;
			for x in arr.iter().skip(1) {
				self.add(',')?;
				self.add(' ')?;
				f(x).show(self)?;
			}
		}
		Ok(self)
	}
}

pub struct WriteShower<W: IoWrite>(W);
impl WriteShower<Stderr> {
	pub fn stderr() -> WriteShower<Stderr> {
		WriteShower::new(stderr())
	}

	pub fn write_stderr<S: Show>(shown: S) -> Result<(), IoError> {
		Self::stderr().add(shown)?.nl()?;
		Ok(())
	}
}
impl<W: IoWrite> WriteShower<W> {
	fn new(w: W) -> Self {
		WriteShower(w)
	}
}
impl<W: IoWrite> Shower for WriteShower<W> {
	type Error = IoError;

	fn _add_char(&mut self, char: char) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", char)
	}

	fn _add_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
		//write!(self.0, "{}", &String::from_utf8_lossy(bytes))
		self.0.write_all(bytes)
	}

	fn _add_u8(&mut self, u: u8) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u)
	}

	fn _add_u32(&mut self, u: u32) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u)
	}

	fn _add_usize(&mut self, u: usize) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", u)
	}

	fn _add_int(&mut self, i: i32) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", i)
	}

	fn _add_float(&mut self, f: f64) -> Result<(), Self::Error> {
		write!(&mut self.0, "{}", f)
	}
}

pub fn serialize_as_show<T : Show, S : Serializer>(t: T, serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&StringMaker::stringify(t))
}
