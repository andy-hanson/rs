use std::borrow::Borrow;
use std::mem::replace;
use std::ops::{Deref, Index, Range};
use std::slice::Iter;
use std::str::FromStr;
use std::vec::IntoIter as VecIntoIter;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Arr<T>(Box<[T]>);

impl<T> Arr<T> {
	pub fn slice(&self, lo: usize, hi: usize) -> &[T] {
		let borrow: &[T] = &*self.0;
		&borrow[lo..hi]
	}

	pub fn into_box(self) -> Box<[T]> {
		self.0
	}

	pub fn _1(a: T) -> Self {
		Arr(Box::new([a]))
	}
	pub fn _2(a: T, b: T) -> Self {
		Arr(Box::new([a, b]))
	}
	pub fn _3(a: T, b: T, c: T) -> Self {
		Arr(Box::new([a, b, c]))
	}
	pub fn _4(a: T, b: T, c: T, d: T) -> Self {
		Arr(Box::new([a, b, c, d]))
	}

	pub fn from_vec(v: Vec<T>) -> Self {
		Arr(v.into_boxed_slice())
	}

	pub fn range(&self) -> Range<usize> {
		0..self.len()
	}

	pub fn empty() -> Self {
		Arr(Vec::new().into_boxed_slice())
	}

	pub fn move_into_iter(self) -> VecIntoIter<T> {
		self.0.into_vec().into_iter()
	}
}
impl<T> Borrow<[T]> for Arr<T> {
	fn borrow(&self) -> &[T] {
		&self.0
	}
}
impl<T> Deref for Arr<T> {
	type Target = [T];
	fn deref(&self) -> &[T] {
		&self.0
	}
}
impl<T: Clone> Arr<T> {
	pub fn into_vec(self) -> Vec<T> {
		let mut v = Vec::new();
		v.clone_from_slice(&self.0);
		v
	}

	pub fn copy_slice(&self, lo: usize, hi: usize) -> Self {
		Arr(self.0[lo..hi].to_owned().into_boxed_slice())
	}

	pub fn rcons(&self, new_last_element: T) -> Self {
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

	pub fn copy_rtail(&self) -> Self {
		let mut b = ArrBuilder::<T>::new();
		for i in self.rtail_range() {
			b.add(self[i].clone())
		}
		b.finish()
	}
}
impl<T: Copy> Arr<T> {
	pub fn copy_from_slice(slice: &[T]) -> Self {
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
	pub fn ends_with(&self, other: &[T]) -> bool {
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
	pub fn copy_from_str(s: &str) -> Self {
		Arr(s.as_bytes().to_owned().into_boxed_slice())
	}
}
pub trait U8SliceOps: SliceOps<u8> {
	fn clone_to_utf8_string(&self) -> String {
		let x = ::std::str::from_utf8(self.as_slice()).unwrap();
		String::from_str(x).unwrap()
		//String::from_utf8(self.clone().into_vec()).unwrap()
	}

	fn as_slice(&self) -> &[u8];

	fn equals_str(&self, s: &str) -> bool {
		//TODO:PERF
		self.each_equals(&Arr::copy_from_str(s), |a, b| a == b)
	}

	fn split_on_char<F: Fn(u8) -> bool>(&self, f: F) -> Arr<Arr<u8>> {
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

	fn without_end_if_ends_with(&self, s: &str) -> &[u8] {
		if self.len() < s.len() {
			return self.as_slice()
		}
		let _ = Arr::copy_from_str(s);
		panic!()
	}
}
impl U8SliceOps for Arr<u8> {
	fn as_slice(&self) -> &[u8] {
		&self.0
	}
}
impl U8SliceOps for [u8] {
	fn as_slice(&self) -> &[u8] {
		self
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
	pub fn new() -> Self {
		ArrBuilder(Vec::new())
	}

	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn new_with_first(first: T) -> Self {
		let mut b = ArrBuilder(Vec::new());
		b.add(first);
		b
	}

	pub fn new_with_optional_first(first: Option<T>) -> Self {
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
impl<T> Deref for ArrBuilder<T> {
	type Target = [T];
	fn deref(&self) -> &[T] {
		&self.0
	}
}


pub trait SliceOps<T>: Index<usize, Output = T> {
	fn len(&self) -> usize;
	fn iter(&self) -> Iter<T>;

	fn only(&self) -> Option<&T> {
		if self.len() == 1 {
			Some(&self[0])
		} else {
			None
		}
	}

	fn map<U, F: FnMut(&T) -> U>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for x in self.iter() {
			b.add(f(x))
		}
		b.finish()
	}

	fn map_with_index<U, F: Fn(&T, usize) -> U>(&self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for i in self.range() {
			b.add(f(&self[i], i))
		}
		b.finish()
	}

	fn map_defined_probably_all<U, F: FnMut(&T) -> Option<U>>(&self, f: F) -> Arr<U> {
		//TODO:PERF we will probably map all, so allocate self.len() space ahead of time
		self.map_defined(f)
	}

	fn map_defined<U, F: FnMut(&T) -> Option<U>>(&self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::<U>::new();
		for x in self.iter() {
			if let Some(y) = f(x) {
				b.add(y)
			}
		}
		b.finish()
	}

	fn find<F: Fn(&T) -> bool>(&self, f: F) -> Option<&T> {
		for x in self.iter() {
			if f(x) {
				return Some(x)
			}
		}
		None
	}

	fn find_index<F: Fn(&T) -> bool>(&self, f: F) -> Option<usize> {
		for (i, x) in self.iter().enumerate() {
			if f(x) {
				return Some(i)
			}
		}
		None
	}

	fn any(&self) -> bool {
		self.len() == 0
	}

	fn last(&self) -> Option<&T> {
		if self.any() {
			Some(&self[self.len() - 1])
		} else {
			None
		}
	}

	fn each_equals<F: Fn(&T, &T) -> bool>(&self, other: &[T], f: F) -> bool {
		self.each_corresponds(other, f)
	}

	fn each_corresponds<U, F: Fn(&T, &U) -> bool>(&self, other: &[U], f: F) -> bool {
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

	fn zip<U, V, F: Fn(&T, &U) -> V>(&self, other: &[U], f: F) -> Arr<V> {
		assert_eq!(self.len(), other.len());
		let mut b = ArrBuilder::<V>::new();
		for i in self.range() {
			b.add(f(&self[i], &other[i]))
		}
		b.finish()
	}

	fn do_zip<U, F: Fn(&T, &U) -> ()>(&self, other: &[U], f: F) {
		assert_eq!(self.len(), other.len());
		for i in self.range() {
			f(&self[i], &other[i])
		}
	}

	fn range(&self) -> Range<usize> {
		0..self.len()
	}
}

pub trait CloneSliceOps<T: Clone>: SliceOps<T> {
	fn copy_to_builder(&self) -> ArrBuilder<T> {
		let mut b = ArrBuilder::<T>::new();
		for x in self.iter() {
			b.add(x.clone())
		}
		b
	}

	fn concat(&self, other: &[T]) -> Arr<T> {
		let mut b = self.copy_to_builder();
		for x in other.iter() {
			b.add(x.clone())
		}
		b.finish()
	}
}

impl<T> SliceOps<T> for Arr<T> {
	fn len(&self) -> usize {
		self.0.len()
	}

	fn iter(&self) -> Iter<T> {
		self.0.into_iter()
	}
}
impl<T: Clone> CloneSliceOps<T> for Arr<T> {}

impl<T> SliceOps<T> for [T] {
	fn len(&self) -> usize {
		self.len()
	}

	fn iter(&self) -> Iter<T> {
		self.into_iter()
	}
}
impl<T: Clone> CloneSliceOps<T> for [T] {}
