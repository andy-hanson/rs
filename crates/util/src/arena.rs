use std::cell::{Cell, UnsafeCell};
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{InPlace, Place, Placer};
use std::ptr::copy_nonoverlapping;
use std::slice;

use super::arith::{isize_to_usize, usize_to_isize};
use super::iter::KnownLen;

pub trait NoDrop {}
impl<T: NoDrop> NoDrop for Option<T> {}
impl<'a, T: NoDrop> NoDrop for &'a [T] {}
impl<'a, T: NoDrop> NoDrop for &'a T {}
impl NoDrop for bool {}
impl NoDrop for u8 {}
impl NoDrop for u32 {}
impl NoDrop for i32 {}
impl NoDrop for f64 {}
impl NoDrop for usize {}

/*
We must use Cell internally and make all methods on Arena take an immutable reference,
// because of this situation:
	let a = Arena::new();
	let x = &mut a <- 1;
	let y = &mut a <- 2;
We would have two mutable borrows for the arena, `x` and `y`, that we need to live simultaneously.
The only way to accompilsh this is to make rust think that the Arena is immutable.
*/
pub struct Arena {
	// Use UnsafeCell because we will mutate the contents.
	bytes: UnsafeCell<Box<[u8]>>,
	next: Cell<*mut u8>,
	end: *mut u8,
	//TODO:cfg[debug]
	locked: Cell<bool>,
}
impl Arena {
	pub fn new() -> Self {
		const LEN: usize = 1_000_000;
		//TODO:PERF: in release mode, get uninitialized memory
		let bytes = UnsafeCell::new(Box::new([0; LEN]));
		let next = unsafe { bytes.get().as_mut() }.unwrap().as_mut_ptr();
		Arena { bytes, next: Cell::new(next), end: unsafe { offset(next, LEN) }, locked: Cell::new(false) }
	}

	fn alloc_in_array<T: NoDrop>(&self) -> *mut T {
		assert!(self.locked.get());
		self.alloc_worker()
	}

	fn alloc_single<T: NoDrop>(&self) -> *mut T {
		self.check_unlocked();
		self.alloc_worker()
	}

	fn check_unlocked(&self) {
		if self.locked.get() {
			panic!("Trying to allocate while in the middle of writing to an array.")
		}
	}

	#[allow(mut_from_ref)]
	pub unsafe fn alloc_uninitialized<T: NoDrop>(&self, len: usize) -> &mut [T] {
		let (_, _, slice) = self.alloc_n::<T>(len);
		slice
	}

	fn alloc_worker<T: NoDrop>(&self) -> *mut T {
		let size = size_of::<T>();
		assert!(size > 0);
		self.alloc_n_bytes(size) as *mut T
	}

	fn alloc_n_bytes(&self, size: usize) -> *mut u8 {
		unsafe {
			let next = self.next.get();
			let new_next = offset(next, size);
			if new_next >= self.end {
				// TODO: Need to alloc more
				unimplemented!()
			}
			self.next.set(new_next);
			next
		}
	}

	unsafe fn alloc_n<T>(&self, len: usize) -> (*mut T, *mut T, &mut [T]) {
		let start = self.alloc_n_bytes(size_of::<T>() * len) as *mut T;
		let end = offset(start, len);
		let slice = slice::from_raw_parts_mut(start, len);
		(start, end, slice)
	}

	#[allow(mut_from_ref)]
	pub fn map<T, U: NoDrop, I: KnownLen<Item = T>, F: FnMut(T) -> U>(
		&self,
		inputs: I,
		mut f: F,
	) -> &mut [U] {
		self.map_with_place(inputs, |x, place| place <- f(x))
	}

	//fn returns the placed value as proof that it initialized the memory.
	#[allow(needless_lifetimes)] // False positive
	pub fn map_with_place<
		'a,
		T,
		U: NoDrop,
		I: KnownLen<Item = T>,
		F: FnMut(T, PointerPlace<'a, U>) -> &'a U,
	>(
		&'a self,
		inputs: I,
		mut f: F,
	) -> &mut [U] {
		unsafe {
			let len = inputs.len();
			let (start, end, slice) = self.alloc_n::<U>(len);
			let mut next = start;
			for x in inputs {
				let place = PointerPlace::new(next);
				let returned_ptr = f(x, place);
				// Should have placed it in the correct location
				assert_eq!(returned_ptr as *const U, next as *const U);
				next = next.offset(1)
			}
			assert_eq!(next, end);
			slice
		}
	}

	pub fn map_defined_probably_all<T, U: NoDrop, I: KnownLen<Item = T>, F: FnMut(T) -> Option<U>>(
		&self,
		input: I,
		mut f: F,
	) -> &[U] {
		unsafe {
			let len = input.len();
			let (start, end, slice) = self.alloc_n::<U>(len);
			let mut next = start;
			for x in input {
				if let Some(u) = f(x) {
					*next = u;
					next = next.offset(1)
				}
			}
			assert!(next <= end);
			slice
		}
	}

	pub fn exact_len_builder<T: NoDrop>(&self, len: usize) -> ExactLenBuilder<T> {
		self.max_len_builder_worker(len, /*is_exact*/ true)
	}

	pub fn max_len_builder<T: NoDrop>(&self, max_len: usize) -> MaxLenBuilder<T> {
		self.max_len_builder_worker(max_len, /*is_exact*/ false)
	}

	fn max_len_builder_worker<T: NoDrop>(&self, max_len: usize, is_exact: bool) -> MaxLenBuilder<T> {
		self.check_unlocked();
		let (start, end, _) = unsafe { self.alloc_n::<T>(max_len) };
		MaxLenBuilder::new(start, end, is_exact)
	}

	// Writes directly into the arena. Other allocations aren't allowed to happen at the same time.
	pub fn direct_builder<T: Copy + NoDrop>(&self) -> DirectBuilder<T> {
		self.locked.set(true);
		DirectBuilder { arena: self, start: self.next.get() as *mut T }
	}

	pub fn copy_slice<T: NoDrop + Copy>(&self, slice: &[T]) -> &[T] {
		unsafe {
			let len = slice.len();
			let (my_start, _, my_slice) = self.alloc_n::<T>(len);
			copy_slice(slice, my_start);
			my_slice
		}
	}
}
impl<'a, 'arena, T: 'a + Sized + NoDrop> Placer<T> for &'a Arena {
	type Place = PointerPlace<'a, T>;

	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.alloc_single())
	}
}

pub type ExactLenBuilder<'a, T> = MaxLenBuilder<'a, T>;
pub struct MaxLenBuilder<'a, T: 'a + Sized + NoDrop> {
	start: *mut T,
	end: *mut T,
	is_exact: bool,
	next: *mut T,
	phantom: PhantomData<&'a T>,
}
impl<'a, T: 'a + Sized + NoDrop> MaxLenBuilder<'a, T> {
	fn new(start: *mut T, end: *mut T, is_exact: bool) -> Self {
		MaxLenBuilder { start, end, is_exact, next: start, phantom: PhantomData }
	}

	pub fn slice_so_far(&self) -> &'a [T] {
		// unwrap() should succeed since we assert T to be non-zero in size.
		unsafe { slice_from_to(self.start, self.next) }
	}

	pub fn finish(self) -> &'a [T] {
		if self.is_exact {
			assert_eq!(self.next, self.end)
		}
		self.slice_so_far()
	}
}
impl<'a, T: 'a + Sized + NoDrop + Copy> MaxLenBuilder<'a, T> {
	pub fn add_slice(&mut self, slice: &[T]) {
		unsafe {
			let old_next = self.next;
			self.next = offset(self.next, slice.len());
			assert!(self.next <= self.end);
			copy_slice(slice, old_next);
		}
	}
}
impl<'a, 'arena, T: 'a + Sized + NoDrop> Placer<T> for &'a mut MaxLenBuilder<'arena, T> {
	type Place = PointerPlace<'arena, T>;

	fn make_place(self) -> Self::Place {
		let place = PointerPlace::new(self.next);
		self.next = unsafe { self.next.offset(1) };
		assert!(self.next <= self.end);
		place
	}
}

// T must be in Copy because we might have to move the array if the arena runs out of length.
pub struct DirectBuilder<'a, T: 'a + Copy + Sized + NoDrop> {
	arena: &'a Arena,
	start: *mut T,
	// Use arena.next as my next pointer
}
impl<'a, T: 'a + Copy + Sized + NoDrop> DirectBuilder<'a, T> {
	pub fn finish(self) -> &'a mut [T] {
		self.arena.locked.set(false);
		unsafe { slice_from_to(self.start, self.arena.next.get() as *mut T) }
	}

	pub fn add_slice(&mut self, slice: &[T]) {
		unsafe {
			let next = self.arena.next.get() as *mut T;
			let new_next = offset(next, slice.len());
			assert!(new_next < (self.arena.end as *mut T));
			self.arena.next.set(new_next as *mut u8);
			copy_slice(slice, next);
		}
	}
}
impl<'builder, 'arena, T: 'arena + Copy + Sized + NoDrop> Placer<T>
	for &'builder mut DirectBuilder<'arena, T> {
	type Place = PointerPlace<'arena, T>;

	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.arena.alloc_in_array())
	}
}

/** Place for a single pointer that's already been allocated. */
pub struct PointerPlace<'a, T: 'a + Sized + NoDrop> {
	ptr: *mut T,
	phantom: PhantomData<&'a T>,
}
impl<'a, T: 'a + Sized + NoDrop> Placer<T> for PointerPlace<'a, T> {
	type Place = Self;

	fn make_place(self) -> Self::Place {
		self
	}
}
impl<'a, T: 'a + Sized + NoDrop> PointerPlace<'a, T> {
	//TODO: not pub
	pub fn new(ptr: *mut T) -> Self {
		PointerPlace { ptr, phantom: PhantomData }
	}
}
impl<'a, T: 'a + Sized + NoDrop> Place<T> for PointerPlace<'a, T> {
	fn pointer(&mut self) -> *mut T {
		self.ptr
	}
}
impl<'a, T: 'a + Sized + NoDrop> InPlace<T> for PointerPlace<'a, T> {
	type Owner = &'a mut T;

	unsafe fn finalize(self) -> Self::Owner {
		// unwrap() should be safe because the pointer should never be null.
		self.ptr.as_mut().unwrap()
	}
}

unsafe fn slice_from_to<'a, T>(start: *mut T, end: *mut T) -> &'a mut [T] {
	let offset_bytes = isize_to_usize(((start as *mut u8).offset_to(end as *mut u8)).unwrap());
	assert_eq!(offset_bytes % size_of::<T>(), 0);
	let len = isize_to_usize(start.offset_to(end).unwrap());
	slice::from_raw_parts_mut(start, len)
}

unsafe fn offset_const<T>(ptr: *const T, offset: usize) -> *const T {
	ptr.offset(usize_to_isize(offset))
}

unsafe fn offset<T>(ptr: *mut T, offset: usize) -> *mut T {
	ptr.offset(usize_to_isize(offset))
}

unsafe fn copy_slice<T>(slice: &[T], start: *mut T) {
	let len = slice.len();
	let end = offset(start, len);
	let slice_start = slice.as_ptr();
	let slice_end = offset_const(slice_start, len);
	assert!((end as *const T) < slice_start || (start as *const T) > slice_end);
	copy_nonoverlapping(/*src*/ slice_start, /*dst*/ start, len)
}
