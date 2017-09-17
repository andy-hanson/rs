use util::arith::{usize_to_u8, u8_to_usize};

use super::Value;

// Every value takes up exactly one word. This may be a pointer.
pub struct ValueStack<'model : 'value, 'value> {
	values: Vec<Value<'model, 'value>>,
}
impl<'model, 'value> ValueStack<'model, 'value> {
	pub fn new() -> Self {
		ValueStack { values: Vec::new() }
	}

	pub fn depth(&self) -> usize {
		self.values.len()
	}

	pub fn pop(&mut self) -> Value<'model, 'value> {
		self.values.pop().unwrap()
	}

	pub fn push(&mut self, value: Value<'model, 'value>) {
		self.values.push(value)
	}

	pub fn fetch(&mut self, by: u8) {
		let index = usize_to_u8(self.values.len()) - 1 - by;
		let value = self.values[u8_to_usize(index)].clone();
		self.push(value)
	}

	pub fn un_let(&mut self, n: u8) {
		let x = self.pop();
		for _ in 0..n {
			self.pop();
		}
		self.push(x);
	}
}
