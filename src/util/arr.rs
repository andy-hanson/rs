use std::ops::{ Index, Range };
use std::slice::Iter;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Arr<T>(Box<[T]>);

impl<T> Arr<T> {
	fn from_vec(v: Vec<T>) -> Arr<T> {
		Arr(v.into_boxed_slice())
	}

	pub fn any(&self) -> bool {
		!self.0.is_empty()
	}

	pub fn range(&self) -> Range<usize> {
		0..self.len()
	}

	pub fn empty() -> Arr<T> {
		Arr(Vec::new().into_boxed_slice())
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn into_iter(&self) -> Iter<T> {
		let x = self.0.into_iter();
		x
	}

	pub fn build_until_null<E, F : FnMut() -> Result<Option<T>, E>>(mut make_option: F) -> Result<Arr<T>, E> {
		let mut b = ArrBuilder::<T>::new();
		loop {
			match make_option()? {
				Some(value) =>
					b.add(value),
				None =>
					break Ok(b.finish())
			}
		}
	}
}
impl<T : Clone> Arr<T> {
	pub fn into_vec(self) -> Vec<T> {
		let mut v = Vec::new();
		v.clone_from_slice(&self.0);
		v
	}

	pub fn copy_slice(&self, lo: usize, hi: usize) -> Arr<T> {
		Arr(self.0[lo..hi].to_owned().into_boxed_slice())
	}
}
impl Arr<u8> {
	pub fn copy_from_str(s: &str) -> Arr<u8> {
		Arr(s.as_bytes().to_owned().into_boxed_slice())
	}

	pub fn clone_to_utf8_string(&self) -> String {
		String::from_utf8(self.clone().into_vec()).unwrap()
	}
}


impl<T> Index<usize> for Arr<T> {
	type Output = T;

	fn index(&self, index: usize) -> &T {
		&self.0[index]
	}
}

pub struct ArrBuilder<T>(Vec<T>);

impl<T> ArrBuilder<T> {
	pub fn new() -> ArrBuilder<T> {
		ArrBuilder(Vec::new())
	}

	pub fn new_with_first(first: T) -> ArrBuilder<T> {
		let mut b = ArrBuilder(Vec::new());
		b.add(first);
		b
	}

	pub fn new_with_optional_first(first: Option<T>) -> ArrBuilder<T> {
		let mut b = ArrBuilder(Vec::new());
		if let Some(x) = first {
			b.add(x)
		}
		b
	}

	pub fn add(&mut self, t: T) {
		self.0.push(t)
	}

	pub fn finish(self) -> Arr<T> {
		Arr::from_vec(self.0)
	}
}
