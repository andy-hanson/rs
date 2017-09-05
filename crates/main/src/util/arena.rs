use std::cell::UnsafeCell;
use std::mem::{size_of};
use std::ops::{Placer, Place, InPlace};
use std::marker::PhantomData;
use std::slice;

use super::arr::{Arr, ArrBuilder};

pub trait NoDrop {
	const NO_DROP_MARKER: u8;
}
impl<T : NoDrop> NoDrop for Option<T> {
	const NO_DROP_MARKER: u8 = 0;
}
impl<'a, T : NoDrop> NoDrop for &'a [T] { const NO_DROP_MARKER:u8 = 0; }
impl<'a, T : NoDrop> NoDrop for &'a T { const NO_DROP_MARKER:u8 = 0;}
impl NoDrop for bool { const NO_DROP_MARKER: u8 = 0; }
impl NoDrop for u8 { const NO_DROP_MARKER: u8 = 0; }
impl NoDrop for u32 { const NO_DROP_MARKER: u8 = 0; }
impl NoDrop for i32 { const NO_DROP_MARKER: u8 = 0; }
impl NoDrop for f64 { const NO_DROP_MARKER: u8 = 0; }
impl NoDrop for usize { const NO_DROP_MARKER: u8 = 0; }

pub struct Arena {
	bytes: UnsafeCell<Box<[u8]>>,
	next_index: UnsafeCell<usize>,
	//TODO:cfg[debug]
	locked: UnsafeCell<bool>,
}
impl Arena {
	pub fn new() -> Self {
		Arena {
			bytes: UnsafeCell::new(Box::new([0; 1000])),
			next_index: UnsafeCell::new(0),
			locked: UnsafeCell::new(false),
		}
	}

	fn cur_index(&self) -> usize {
		unsafe { *self.next_index.get() }
	}

	fn alloc_in_array<T : NoDrop>(&self) -> *mut T {
		self.alloc_worker()
	}

	fn alloc_single<T : NoDrop>(&self) -> *mut T {
		if unsafe { *self.locked.get() } {
			panic!("Trying to allocate while in the middle of writing to an array.")
		}
		self.alloc_worker()
	}

	fn alloc_worker<T : NoDrop>(&self) -> *mut T {
		let size = size_of::<T>();
		assert!(size > 0);
		unsafe {
			let bytes = &mut *self.bytes.get();
			let next_index = self.next_index.get();
			let pt = bytes.as_mut_ptr().offset(*next_index as isize) as *mut T;
			*next_index += size;
			assert!(*next_index < bytes.len());
			&mut *pt
		}
	}

	pub fn clone_slice<T : NoDrop + Copy>(&self, slice: &[T]) -> &[T] {
		//TODO:PERF faster copy
		let b = self.direct_arr_builder::<T>();
		for em in slice {
			&b <- *em;
		}
		b.finish()
	}

	// Writes directly into the arena. Other allocations aren't allowed to happen at the same time.
	pub fn direct_arr_builder<T : NoDrop>(&self) -> DirectArrBuilder<T> {
		unsafe { *self.locked.get() = true; }
		let start_byte_index = self.cur_index();
		DirectArrBuilder {
			arena: self,
			phantom: PhantomData,
			start_ptr: unsafe {
				(*self.bytes.get()).as_mut_ptr().offset(start_byte_index as isize) as *mut T
			},
			start_byte_index,
		}
	}

	pub fn list_builder<'a, T : NoDrop>(&'a self) -> ListBuilder<'a, T> {
		ListBuilder::new(&self)
	}

	// Writes to a buffer and copies to the arena when finishing.
	/*pub fn safe_arr_builder<T : NoDrop>(&self) -> SafeArrBuilder<T> {
		SafeArrBuilder {
			arena: self,
			vec: Vec::new(),
		}
	}*/
}
impl<'a, 'arena, T : 'a + Sized + NoDrop> Placer<T> for &'a Arena {
	type Place = PointerPlace<'a, T>;

	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.alloc_single())
	}
}

pub struct ListBuilder<'a, T : 'a + Sized + NoDrop> {
	arena: &'a Arena,
	//Returned at the end
	first: UnsafeCell<Option<*const ListNode<'a, T>>>,
	//Used to append new elements
	last: UnsafeCell<Option<*mut ListNode<'a, T>>>,
	length: UnsafeCell<usize>,
}
impl<'a, T : 'a + Sized + NoDrop> ListBuilder<'a, T> {
	fn new(arena: &'a Arena) -> Self {
		ListBuilder {
			arena,
			first: UnsafeCell::new(None),
			last: UnsafeCell::new(None),
			length: UnsafeCell::new(0),
		}
	}

	pub fn add(&mut self) -> AddPlacer<T> {
		//TODO:duplciate code in make_place
		unsafe {
			*self.length.get().as_mut().unwrap() += 1;
			let new_last = self.arena.alloc_single::<ListNode<'a, T>>();
			let first = self.first.get().as_mut().unwrap();
			let last = self.last.get().as_mut().unwrap();
			if first.is_none() {
				//First node
				*first = Some(new_last);
				*last = Some(new_last);
			} else {
				//If `first` is Some, `last` must be too.
				*(*last.unwrap()).next.get().as_mut().unwrap() = Some(&*new_last);
				*last = Some(new_last);
				//let old_last = replace(unsafe { self.last.get().as_mut() }.unwrap(), None);
				//old_last.next = new_last;
				//.next = old_head.map(|ptr| unsafe { &*ptr });
				//*self.last.get() = Some(node);
			}
			AddPlacer(&mut (*new_last).value)
		}
	}

	pub fn finish(self) -> List<'a, T> {
		unsafe {
			List {
				len: *self.length.get(),
				head: (*self.first.get()).map(|ptr| &*ptr),
			}
		}
	}
}

pub struct AddPlacer<'a, T : 'a + Sized + NoDrop>(&'a mut T);
impl<'a, T : 'a + Sized + NoDrop> Placer<T> for AddPlacer<'a, T> {
	type Place = PointerPlace<'a, T>;
	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.0)
	}
}

pub struct List<'a, T : 'a> {
	pub len: usize,
	head: Option<&'a ListNode<'a, T>>,
}
impl<'a, T : 'a> List<'a, T> {
	pub fn empty() -> Self {
		List { len: 0, head: None }
	}

	pub fn any(&self) -> bool {
		self.len != 0
	}

	//TODO:KILL
	pub fn map<U, F : FnMut(&'a T) -> U>(&'a self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::<U>::new();
		for x in self.iter() {
			b.add(f(x))
		}
		b.finish()
	}

	//TODO:KILL
	pub fn map_defined_probably_all<U, F: FnMut(&'a T) -> Option<U>>(&'a self, f: F) -> Arr<U> {
		//TODO:PERF we will probably map all, so allocate self.len() space ahead of time
		self.map_defined(f)
	}

	//TODO:KILL
	pub fn map_defined<U, F: FnMut(&'a T) -> Option<U>>(&'a self, mut f: F) -> Arr<U> {
		let mut b = ArrBuilder::<U>::new();
		for x in self.iter() {
			if let Some(y) = f(x) {
				b.add(y)
			}
		}
		b.finish()
	}

	//TODO:KILL
	pub fn do_zip<'u, U, F: Fn(&'a T, &'u U) -> ()>(&'a self, other: &'u [U], f: F) {
		assert_eq!(self.len, other.len());
		let mut i = 0;
		for x in self.iter() {
			let o = &other[i];
			i += 1;
			f(x, o)
		}
	}

	//TODO:KILL
	pub fn map_with_index<U, F: Fn(&'a T, usize) -> U>(&'a self, f: F) -> Arr<U> {
		let mut b = ArrBuilder::new();
		for (i, x) in self.iter().enumerate() {
			b.add(f(x, i))
		}
		b.finish()
	}

	pub fn iter(&self) -> ListIter<'a, T> {
		ListIter(self.head)
	}
}
impl<'a, T> NoDrop for List<'a, T> {
	const NO_DROP_MARKER: u8 = 0;
}

pub struct ListIter<'a, T : 'a>(Option<&'a ListNode<'a, T>>);
impl<'a, T : 'a> Iterator for ListIter<'a, T> {
	type Item = &'a T;

	fn next(&mut self) -> Option<&'a T> {
		self.0.map(|n| {
			let value = &n.value;
			self.0 = unsafe { *n.next.get() };
			value
		})
	}
}

struct ListNode<'a, T : 'a> {
	value: T,
	next: UnsafeCell<Option<&'a ListNode<'a, T>>>,
}
impl<'a, T> NoDrop for ListNode<'a, T> where T : NoDrop {
	const NO_DROP_MARKER: u8 = 0;
}

//KILL
/*pub struct SafeArrBuilder<'a, T : 'a + Sized + NoDrop> {
	arena: &'a Arena,
	vec: Vec<T>,
}
impl<'a, T : 'a + Clone + Sized + NoDrop> SafeArrBuilder<'a, T> {
	//TODO:PERF emplace
	pub fn add(&mut self, value: T) {
		self.vec.push(value)
	}

	pub fn finish(self) -> &'a [T] {
		self.arena.clone_slice(&self.vec)
	}
}*/

pub struct DirectArrBuilder<'a, T : 'a + Sized + NoDrop> {
	arena: &'a Arena,
	phantom: PhantomData<&'a T>,
	start_ptr: *mut T,
	start_byte_index: usize,
}
impl<'a, T : 'a + Sized + NoDrop> DirectArrBuilder<'a, T> {
	pub fn finish(self) -> &'a mut [T] {
		unsafe { *self.arena.locked.get() = false; }

		let len_bytes = self.arena.cur_index() - self.start_byte_index;
		let t_size = size_of::<T>();
		assert!(len_bytes % t_size == 0);
		let len = len_bytes / t_size;
		unsafe { slice::from_raw_parts_mut(self.start_ptr, len) }
	}
}
impl<'a, T : 'a + Sized + NoDrop> Placer<T> for &'a DirectArrBuilder<'a, T> {
	type Place = PointerPlace<'a, T>;
	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.arena.alloc_in_array())
	}
}

/** Place for a single pointer that's already been allocated. */
pub struct PointerPlace<'a, T : 'a + Sized + NoDrop> {
	ptr: *mut T,
	phantom: PhantomData<&'a T>
}
impl<'a, T : 'a + Sized + NoDrop> PointerPlace<'a, T> {
	fn new(ptr: *mut T) -> Self {
		PointerPlace { ptr, phantom: PhantomData }
	}
}
impl<'a, T : 'a + Sized + NoDrop> Place<T> for PointerPlace<'a, T> {
	fn pointer(&mut self) -> *mut T {
		self.ptr
	}
}
impl<'a, T : 'a + Sized + NoDrop> InPlace<T> for PointerPlace<'a, T> {
	type Owner = &'a mut T;

	unsafe fn finalize(self) -> Self::Owner {
		// unwrap() should be safe because the pointer should never be null.
		self.ptr.as_mut().unwrap()
	}
}
