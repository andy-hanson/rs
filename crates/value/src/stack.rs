use util::arith::{u8_to_usize, usize_to_u8};

use super::value::Value;

// Every value takes up exactly one word. This may be a pointer.
pub struct ValueStack<'model: 'value, 'value> {
	values: Vec<Value<'model, 'value>>,
}
impl<'model, 'value> ValueStack<'model, 'value> {
	pub fn new() -> Self {
		ValueStack { values: Vec::new() }
	}

	// Does not actually take it off, just shows it as a slice.
	pub fn get_slice_of_top(&self, n: usize) -> &[Value<'model, 'value>] {
		&self.values[self.values.len() - n..]
	}

	pub fn depth(&self) -> usize {
		self.values.len()
	}

	pub fn drop(&mut self, n: usize) {
		for _ in 0..n {
			self.values.pop().unwrap();
		}
	}

	pub fn pop(&mut self) -> Value<'model, 'value> {
		self.values.pop().unwrap()
	}

	pub fn push(&mut self, value: Value<'model, 'value>) {
		self.values.push(value)
	}

	pub fn fetch(&mut self, by: u8) {
		let index = usize_to_u8(self.values.len()) - by;
		let value = self.values[u8_to_usize(index)];
		self.push(value)
	}

	pub fn un_let(&mut self, n: u8) {
		let returned = self.pop();
		for _ in 0..n {
			self.pop();
		}
		self.push(returned);
	}
}
