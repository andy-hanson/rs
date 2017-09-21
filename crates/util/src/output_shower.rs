use std::io::{Error as IoError, Write};

use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

use super::show::{Color, Show, Shower};

pub struct OutputShower(StandardStream);
impl OutputShower {
	pub fn stdout() -> OutputShower {
		OutputShower(StandardStream::stdout(ColorChoice::Always))
	}

	pub fn stderr() -> OutputShower {
		OutputShower(StandardStream::stderr(ColorChoice::Always))
	}

	pub fn write_stderr<S: Show>(shown: S) -> Result<(), IoError> {
		Self::stderr().add(shown)?.nl()?;
		Ok(())
	}
}
impl Shower for OutputShower {
	type Error = IoError;

	fn color<F: FnMut() -> Result<(), Self::Error>>(
		&mut self,
		c: Color,
		mut f: F,
	) -> Result<(), <Self as Shower>::Error> {
		self.0.set_color(ColorSpec::new().set_fg(Some(c)))?;
		f()?;
		self.0
			.set_color(ColorSpec::new().set_fg(Some(Color::White))) //TODO: use prior color
	}

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
