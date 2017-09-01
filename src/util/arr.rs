use std::mem::replace;
use std::ops::{Index, Range};
use std::slice::Iter;
use std::vec::IntoIter as VecIntoIter;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Arr<T>(Box<[T]>);

impl<T> Arr<T> {
	pub fn only(&self) -> Option<&T> {
		if self.len() == 1 {
			Some(&self[0])
		} else {
			None
		}
	}

	pub fn as_slice(&self) -> &[T] {
		&*self.0
	}

	pub fn slice(&self, lo: usize, hi: usize) -> &[T] {
		&self.as_slice()[lo..hi]
	}

	pub fn into_box(self) -> Box<[T]> {
		self.0
	}

	pub fn _1(a: T) -> Arr<T> {
		Arr(Box::new([a]))
	}
	pub fn _2(a: T, b: T) -> Arr<T> {
		Arr(Box::new([a, b]))
	}
	pub fn _3(a: T, b: T, c: T) -> Arr<T> {
		Arr(Box::new([a, b, c]))
	}
	pub fn _4(a: T, b: T, c: T, d: T) -> Arr<T> {
		Arr(Box::new([a, b, c, d]))
	}

	pub fn from_vec(v: Vec<T>) -> Arr<T> {
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

	pub fn move_into_iter(self) -> VecIntoIter<T> {
		self.0.into_vec().into_iter()
	}

	pub fn iter(&self) -> Iter<T> {
		self.0.into_iter()
	}

	pub fn map<U, F: FnMut(&T) -> U>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for x in self.iter() {
			b.add(f(x))
		}
		b.finish()
	}

	pub fn map_with_index<U, F: Fn(&T, usize) -> U>(&self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for i in self.range() {
			b.add(f(&self[i], i))
		}
		b.finish()
	}

	pub fn map_defined_probably_all<U, F: FnMut(&T) -> Option<U>>(&self, f: F) -> Arr<U> {
		//TODO:PERF we will probably map all, so allocate self.len() space ahead of time
		self.map_defined(f)
	}

	pub fn map_defined<U, F: FnMut(&T) -> Option<U>>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::<U>::new();
		for x in self.iter() {
			if let Some(y) = f(x) {
				b.add(y)
			}
		}
		b.finish()
	}

	pub fn find<F: Fn(&T) -> bool>(&self, f: F) -> Option<&T> {
		for x in self.iter() {
			if f(x) {
				return Some(x)
			}
		}
		None
	}

	pub fn find_index<F: Fn(&T) -> bool>(&self, f: F) -> Option<usize> {
		for i in self.range() {
			if f(&self[i]) {
				return Some(i)
			}
		}
		None
	}

	pub fn last(&self) -> Option<&T> {
		if self.any() {
			Some(&self[self.len() - 1])
		} else {
			None
		}
	}

	pub fn zip<U, V, F: Fn(&T, &U) -> V>(&self, other: &Arr<U>, f: F) -> Arr<V> {
		assert_eq!(self.len(), other.len());
		let mut b = ArrBuilder::<V>::new();
		for i in self.range() {
			b.add(f(&self[i], &other[i]))
		}
		b.finish()
	}

	pub fn do_zip<U, F: Fn(&T, &U) -> ()>(&self, other: &Arr<U>, f: F) {
		assert_eq!(self.len(), other.len());
		for i in self.range() {
			f(&self[i], &other[i])
		}
	}

	pub fn each_equals<F: Fn(&T, &T) -> bool>(&self, other: &Arr<T>, f: F) -> bool {
		self.each_corresponds(other, f)
	}

	pub fn each_corresponds<U, F: Fn(&T, &U) -> bool>(&self, other: &Arr<U>, f: F) -> bool {
		if self.len() != other.len() {
			return false
		}
		for i in self.range() {
			if !f(&self[i], &other[i]) {
				return false
			}
		}
		true
	}
}
impl<T: Clone> Arr<T> {
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

	fn rtail_range(&self) -> Range<usize> {
		0..self.len() - 1
	}

	pub fn slice_rtail(&self) -> &[T] {
		&self.0[self.rtail_range()]
	}

	pub fn copy_rtail(&self) -> Arr<T> {
		let mut b = ArrBuilder::<T>::new();
		for i in self.rtail_range() {
			b.add(self[i].clone())
		}
		b.finish()
	}
}
impl<T: Copy> Arr<T> {
	pub fn from_slice(slice: &[T]) -> Arr<T> {
		// TODO: better way?
		let mut v = Vec::new();
		v.copy_from_slice(slice);
		Arr(v.into_boxed_slice())
	}

	pub fn map_on_copies<U, F: Fn(T) -> U>(&self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for x in self.iter() {
			b.add(f(*x))
		}
		b.finish()
	}
}

impl<T: Eq> Arr<T> {
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

//TODO: kill?
impl Arr<u8> {
	pub fn copy_from_str(s: &str) -> Arr<u8> {
		Arr(s.as_bytes().to_owned().into_boxed_slice())
	}

	pub fn clone_to_utf8_string(&self) -> String {
		String::from_utf8(self.clone().into_vec()).unwrap()
	}

	pub fn equals_str(&self, s: &str) -> bool {
		//TODO:PERF
		self.each_equals(&Arr::copy_from_str(s), |a, b| a == b)
	}

	pub fn split<F: Fn(u8) -> bool>(&self, f: F) -> Arr<Arr<u8>> {
		let mut parts = ArrBuilder::<Arr<u8>>::new();
		let mut b = ArrBuilder::<u8>::new();
		for ch in self.iter() {
			if f(*ch) {
				let old_b = replace(&mut b, ArrBuilder::new());
				parts.add(old_b.finish())
			} else {
				b.add(*ch)
			}
		}
		parts.add(b.finish());
		parts.finish()
	}

	pub fn without_end_if_ends_with(&self, s: &str) -> Arr<u8> {
		if self.len() < s.len() {
			return self.clone()
		}
		let _ = Arr::copy_from_str(s);
		panic!()
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

	pub fn as_slice(&self) -> &[T] {
		self.0.as_slice()
	}

	pub fn len(&self) -> usize {
		self.0.len()
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
