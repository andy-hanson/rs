use std::iter::Iterator;
use std::ops::{Index};
use std::slice::Iter;
use std::str::FromStr;

use super::arena::{Arena, List, NoDrop};

/*impl<T: Clone> Arr<T> {
	pub fn clone_to_vec(&self) -> Vec<T> {
		self.0.to_vec()
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
}*/

/*impl<T: Eq> Arr<T> {
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
impl<T: Serialize> Serialize for Arr<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let mut seq = serializer.serialize_seq(Some(self.len()))?;
		for e in self.iter() {
			seq.serialize_element(e)?;
		}
		seq.end()
	}
}*/


pub trait U8SliceOps: SliceOps<u8> {
	fn clone_to_utf8_string(&self) -> String {
		//TODO:PERF
		let x = ::std::str::from_utf8(self.as_slice()).unwrap();
		String::from_str(x).unwrap()
	}

	fn as_slice(&self) -> &[u8];

	fn equals_str(&self, s: &str) -> bool {
		unused!(s);
		//TODO:PERF
		//self.each_equals(&Arr::copy_from_str(s), |a, b| a == b)
		unimplemented!()
	}

	fn without_end_if_ends_with(&self, end: &[u8]) -> &[u8] {
		if self.len() < end.len() {
			self.as_slice()
		} else {
			unimplemented!()
		}
	}
}
impl U8SliceOps for [u8] {
	fn as_slice(&self) -> &[u8] {
		self
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

	fn some<F: Fn(&T) -> bool>(&self, pred: F) -> bool {
		for x in self.iter() {
			if pred(x) {
				return true
			}
		}
		false
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
		self.len() != 0
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
		for (i, x) in self.iter().enumerate() {
			if !f(&x, &other[i]) {
				return false
			}
		}
		true
	}

	//TODO:PERF (only iterate once)
	fn each_corresponds_list<'a, U : NoDrop, F: Fn(&T, &U) -> bool>(&self, other: &'a List<'a, U>, f: F) -> bool {
		if self.len() != other.len {
			return false
		}
		for (i, o) in other.iter().enumerate() {
			if !f(&self[i], o) {
				return false
			}
		}
		true
	}

	fn map<'a, 'out, U, F : FnMut(&'a T) -> U>(&'a self, arena: &'out Arena, f: F) -> &'out [U] where T : 'a {
		arena.map_from(self.len(), self.iter(), f)
	}

	fn zip<'slice, 'out, U, V : NoDrop, I : Iterator<Item=U>, F: Fn(&'slice T, U) -> V>(&'slice self, other: I, arena: &'out Arena, f: F) -> &'out [V] where T : 'slice {
		self.try_zip(other, arena, f).unwrap()
	}

	fn try_zip<'slice, 'out, U, V : NoDrop, I : Iterator<Item=U>, F: Fn(&'slice T, U) -> V>(&'slice self, other: I, arena: &'out Arena, f: F) -> Result<&'out [V], (usize, usize)> where T : 'slice {
		let b = arena.max_size_arr_builder::<V>(self.len());
		let mut my_iter = self.iter();
		let mut other_iter = other.into_iter();
		//let mut len = 0;
		loop {
			match my_iter.next() {
				Some(x) => {
					match other_iter.next() {
						Some(y) => {
							&b <- f(x, y);
						}
						None => unimplemented!()
					}
				}
				None => {
					match other_iter.next() {
						Some(_) => unimplemented!(),
						None => break Ok(b.finish())
					}
				}
			}
			//len += 1;
		}
	}
}
impl<T> SliceOps<T> for [T] {
	fn len(&self) -> usize {
		self.len()
	}

	fn iter(&self) -> Iter<T> {
		self.into_iter()
	}
}

pub fn single_as_slice<T>(item: &T) -> &[T] {
	unsafe { ::std::slice::from_raw_parts(item, 1) }
}

pub trait EqSliceOps<T : Eq> : SliceOps<T> {
	fn count(&self, value: T) -> usize {
		let mut count = 0;
		for x in self.iter() {
			if *x == value {
				count += 1
			}
		}
		count
	}
}
impl<T : Eq> EqSliceOps<T> for [T] {}
