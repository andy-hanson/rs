use util::arith::{to_u8, u8_to_usize};

use super::super::value::Value;

pub struct DataStack(Vec<Value>);
impl DataStack {
	pub fn new(initial: Vec<Value>) -> DataStack {
		DataStack(initial)
	}

	pub fn depth(&self) -> usize {
		self.0.len()
	}

	pub fn pop(&mut self) -> Value {
		self.0.pop().unwrap()
	}

	pub fn push(&mut self, value: Value) {
		self.0.push(value)
	}

	pub fn fetch(&mut self, by: u8) {
		let index = to_u8(self.0.len()) - 1 - by;
		let value = self.0[u8_to_usize(index)].clone();
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
