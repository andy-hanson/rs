use std::ops::Placer;
use std::vec::PlaceBack;

use util::arith::{usize_to_u8, u8_to_usize};

use super::super::value::Value;

pub struct DataStack<'model>(Vec<Value<'model>>);
impl<'model> DataStack<'model> {
	pub fn new(initial: Vec<Value<'model>>) -> Self {
		DataStack(initial)
	}

	pub fn depth(&self) -> usize {
		self.0.len()
	}

	pub fn pop(&mut self) -> Value<'model> {
		self.0.pop().unwrap()
	}

	pub fn push(&mut self, value: Value<'model>) {
		self.0.push(value)
	}

	pub fn fetch(&mut self, by: u8) {
		let index = usize_to_u8(self.0.len()) - 1 - by;
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
impl<'a, 'model> Placer<Value<'model>> for &'a mut DataStack<'model> {
	type Place = PlaceBack<'a, Value<'model>>;

	fn make_place(self) -> Self::Place {
		self.0.place_back().make_place()
	}
}
