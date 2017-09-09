use serde::{Serialize, Serializer};

use std::cell::{Cell, UnsafeCell};
use std::hash::{Hash, Hasher};
use std::mem::size_of;
use std::ops::{Placer, Place, InPlace, Deref};
use std::marker::PhantomData;
use std::slice;

use super::arith::{isize_to_usize, usize_to_isize};

pub trait NoDrop {}
impl<T : NoDrop> NoDrop for Option<T> {}
impl<'a, T : NoDrop> NoDrop for &'a [T] {}
impl<'a, T : NoDrop> NoDrop for &'a T {}
impl NoDrop for bool {}
impl NoDrop for u8 {}
impl NoDrop for u32 {}
impl NoDrop for i32 {}
impl NoDrop for f64 {}
impl NoDrop for usize {}

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
		self.check_lock();
		self.alloc_worker()
	}

	fn check_lock(&self) {
		if unsafe { *self.locked.get() } {
			panic!("Trying to allocate while in the middle of writing to an array.")
		}
	}

	fn alloc_worker<T : NoDrop>(&self) -> *mut T {
		let size = size_of::<T>();
		assert!(size > 0);
		self.alloc_n_bytes(size) as *mut T
	}

	fn alloc_n_bytes(&self, size: usize) -> *mut u8 {
		unsafe {
			let bytes = &mut *self.bytes.get();
			let next_index = self.next_index.get();
			let pt = bytes.as_mut_ptr().offset(usize_to_isize(*next_index));
			*next_index += size;
			assert!(*next_index < bytes.len());
			pt
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

	fn alloc_n<T>(&self, len: usize) -> *mut T {
		self.alloc_n_bytes(size_of::<T>() * len) as *mut T
	}

	pub fn map_from<T, U, I : Iterator<Item=T>, F : FnMut(T) -> U>(&self, len: usize, iter: I, mut f: F) -> &[U] {
		//TODO: don't even need to check the max size
		let start = self.alloc_n(len);
		let mut next = start;
		unsafe {
			let max = next.offset(usize_to_isize(len));
			for x in iter {
				*next = f(x);
				next = next.offset(1)
			}
			assert_eq!(next, max);
			slice::from_raw_parts(start, len)
		}
	}

	pub fn max_size_arr_builder<T : NoDrop>(&self, max_len: usize) -> MaxSizeArrBuilder<T> {
		self.check_lock();
		let ptr = self.alloc_n_bytes(size_of::<T>() * max_len) as *mut T;
		MaxSizeArrBuilder::new(ptr, max_len)
	}

	// Writes directly into the arena. Other allocations aren't allowed to happen at the same time.
	pub fn direct_arr_builder<T : NoDrop>(&self) -> DirectArrBuilder<T> {
		unsafe { *self.locked.get() = true; }
		let start_byte_index = self.cur_index();
		DirectArrBuilder {
			arena: self,
			start_ptr: unsafe {
				(*self.bytes.get()).as_mut_ptr().offset(usize_to_isize(start_byte_index)) as *mut T
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
	len: UnsafeCell<usize>,
}
impl<'a, T : 'a + Sized + NoDrop> ListBuilder<'a, T> {
	fn new(arena: &'a Arena) -> Self {
		ListBuilder {
			arena,
			first: UnsafeCell::new(None),
			last: UnsafeCell::new(None),
			len: UnsafeCell::new(0),
		}
	}

	pub fn add(&self) -> AddPlacer<T> {
		//TODO:duplciate code in make_place
		unsafe {
			*self.len.get().as_mut().unwrap() += 1;
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
				len: *self.len.get(),
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

pub struct List<'a, T : NoDrop + 'a> {
	pub len: usize,
	head: Option<&'a ListNode<'a, T>>,
}
impl<'a, T : NoDrop + 'a> List<'a, T> {
	pub fn empty() -> Self {
		List { len: 0, head: None }
	}

	pub fn single(value: T, arena: &'a Arena) -> Self {
		List {
			len: 1,
			head: Some(arena <- ListNode {
				value,
				next: UnsafeCell::new(None),
			})
		}
	}

	pub fn any(&self) -> bool {
		self.len != 0
	}

	pub fn map<'out, U, F : FnMut(&'a T) -> U>(&'a self, arena: &'out Arena, f: F) -> &'out [U] {
		arena.map_from(self.len, self.iter(), f)
	}

	//TODO:KILL
	pub fn map_defined_probably_all<'out, U, F: FnMut(&'a T) -> Option<U>>(&'a self, arena: &'out Arena, f: F) -> &'out [U] {
		unused!(arena, f);
		//TODO:PERF we will probably map all, so allocate self.len() space ahead of time
		unimplemented!()
	}

	//TODO:KILL
	pub fn map_defined<'out, U, F: FnMut(&'a T) -> Option<U>>(&'a self, f: F) -> &'out [U] {
		unused!(f);
		unimplemented!()
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
	pub fn map_with_index<'out, U, F: Fn(&'a T, usize) -> U>(&'a self, arena: &'out Arena, f: F) -> &'out [U] {
		unused!(arena, f);
		unimplemented!()
	}

	pub fn iter(&self) -> ListIter<'a, T> {
		ListIter(self.head)
	}
}
impl<'a, T : NoDrop + 'a> NoDrop for List<'a, T> {}

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

//Just a marker for an up-pointer. Not serializable (normally, anyway.)
#[derive(Copy, Clone)]
pub struct Up<'a, T : 'a>(pub &'a T);
impl<'a, T : NoDrop> NoDrop for Up<'a, T> {}
impl<'a, T> Up<'a, T> {
	//TODO: shouldn't be needed, this is Copy!
	pub fn clone_as_up(&self) -> Up<'a, T> {
		Up(self.0)
	}
}
impl<'a, T> Deref for Up<'a, T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.0
	}
}
impl<'a, T: SerializeUp> Serialize for Up<'a, T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		(*self).serialize_up(serializer)
	}
}
impl<'a, T> Eq for Up<'a, T> {}
impl<'a, T> Hash for Up<'a, T> {
	fn hash<H : Hasher>(&self, state: &mut H) {
		state.write_usize(self.0 as *const T as usize)
	}
}
impl<'a, T> PartialEq for Up<'a, T> {
	fn eq(&self, other: &Self) -> bool {
		ptr_eq(self.0, other.0)
	}
}

pub fn ptr_eq<T>(a: &T, b: &T) -> bool {
	a as *const T == b as *const T
}

// Like Serialize, but since this is just a *reference* to the data,
// don't serialize everything, just e.g. the name.
pub trait SerializeUp {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer;
}

struct ListNode<'a, T : 'a> {
	value: T,
	next: UnsafeCell<Option<&'a ListNode<'a, T>>>,
}
impl<'a, T> NoDrop for ListNode<'a, T> where T : NoDrop {}

pub struct MaxSizeArrBuilder<'a, T : 'a + Sized + NoDrop> {
	start_ptr: *mut T,
	max_ptr: *mut T,
	next_ptr: Cell<*mut T>,
	phantom: PhantomData<&'a T>,
}
impl<'a, T : 'a + Sized + NoDrop> MaxSizeArrBuilder<'a, T> {
	fn new(start_ptr: *mut T, max_len: usize) -> Self {
		MaxSizeArrBuilder {
			start_ptr,
			max_ptr: unsafe { start_ptr.offset(usize_to_isize(max_len)) },
			next_ptr: Cell::new(start_ptr),
			phantom: PhantomData
		}
	}

	pub fn slice_so_far(&self) -> &'a [T] {
		// unwrap() should succeed since we assert T to be non-zero in size.
		let len = isize_to_usize(self.start_ptr.offset_to(self.next_ptr.get()).unwrap());
		unsafe { slice::from_raw_parts(self.start_ptr, len) }
	}

	pub fn finish(self) -> &'a [T] {
		self.slice_so_far()
	}
}
impl<'a, T : 'a + Sized + NoDrop + Copy> MaxSizeArrBuilder<'a, T> {
	pub fn add_slice(&self, slice: &[T]) {
		//TODO:PERF fast copy
		for x in slice {
			self <- *x;
		}
	}
}
impl<'a, 'arena, T : 'a + Sized + NoDrop> Placer<T> for &'a MaxSizeArrBuilder<'arena, T> {
	type Place = PointerPlace<'arena, T>;

	fn make_place(self) -> Self::Place {
		let place = PointerPlace::new(self.next_ptr.get());
		let new_next = unsafe { self.next_ptr.get().offset(1) };
		self.next_ptr.set(new_next);
		assert!(new_next <= self.max_ptr);
		place
	}
}

pub struct DirectArrBuilder<'a, T: Sized + NoDrop> {
	arena: &'a Arena,
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
impl<'a, T : 'a + Sized + NoDrop + Copy> DirectArrBuilder<'a, T> {
	pub fn add_slice(&self, slice: &[T]) {
		//TODO:PERF fast copy
		for x in slice {
			self <- *x;
		}
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
