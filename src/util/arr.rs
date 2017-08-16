use std::ops::{ Index, Range };
use std::slice::Iter;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Arr<T>(Box<[T]>);

impl<T> Arr<T> {
	pub fn pair(a: T, b: T) -> Arr<T> {
		Arr(Box::new([a, b]))
	}

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

	pub fn iter(&self) -> Iter<T> {
		self.0.into_iter()
	}

	pub fn map<U, F : FnMut(&T) -> U>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for x in self.iter() {
			b.add(f(x))
		}
		b.finish()
	}

	pub fn map_with_index<U, F : Fn(&T, usize) -> U>(&self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for i in self.range() {
			b.add(f(&self[i], i))
		}
		b.finish()
	}

	pub fn map_defined_probably_all<U, F : FnMut(&T) -> Option<U>>(&self, f: F) -> Arr<U> {
		//TODO:PERF we will probably map all, so allocate self.len() space ahead of time
		self.map_defined(f)
	}

	pub fn map_defined<U, F : FnMut(&T) -> Option<U>>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::<U>::new();
		for x in self.iter() {
			if let Some(y) = f(x) {
				b.add(y)
			}
		}
		b.finish()
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

	pub fn last(&self) -> Option<&T> {
		if self.any() {
			Some(&self[self.len() - 1])
		} else {
			None
		}
	}

	pub fn zip<U, V, F : Fn(&T, &U) -> V>(&self, other: &Arr<U>, f: F) -> Arr<V> {
		assert!(self.len() == other.len());
		let mut b = ArrBuilder::<V>::new();
		for i in self.range() {
			b.add(f(&self[i], &other[i]))
		}
		b.finish()
	}

	pub fn do_zip<U, F : Fn(&T, &U) -> ()>(&self, other: &Arr<U>, f: F) {
		assert!(self.len() == other.len());
		for i in self.range() {
			f(&self[i], &other[i])
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

	pub fn copy_to_builder(&self) -> ArrBuilder<T> {
		let mut b = ArrBuilder::<T>::new();
		for x in self.iter() {
			b.add(x.clone())
		}
		b
	}

	pub fn concat(&self, other: &Arr<T>) -> Arr<T> {
		let mut b = self.copy_to_builder();
		for x in other.iter() {
			b.add(x.clone())
		}
		b.finish()
	}

	pub fn rcons(&self, new_last_element: T) -> Arr<T> {
		let mut b = self.copy_to_builder();
		b.add(new_last_element);
		b.finish()
	}

	pub fn slice_rtail(&self) -> &[T] {
		&self.0[0..self.len() - 1]
	}

	pub fn copy_rtail(&self) -> Arr<T> {
		let mut b = ArrBuilder::<T>::new();
		for i in 0..self.len() - 1 {
			b.add(self[i].clone())
		}
		b.finish()
	}
}
impl<T : Copy> Arr<T> {
	pub fn map_on_copies<U, F : Fn(T) -> U>(&self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for x in self.iter() {
			b.add(f(x.clone()))
		}
		b.finish()
	}
}

impl<T : Eq> Arr<T> {
	pub fn ends_with(&self, other: &Arr<T>) -> bool {
		if self.len() < other.len() {
			return false
		}

		for i in other.range() {
			if self[self.len() - i] != other[other.len() - i] {
				return false
			}
		}

		true
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
