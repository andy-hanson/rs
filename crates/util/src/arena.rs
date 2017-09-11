use std::cell::{Cell, UnsafeCell};
use std::fs::File;
use std::io::{Read, Result as IoResult};
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::{InPlace, Place, Placer};
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

pub struct Arena {
	bytes: UnsafeCell<Box<[u8]>>,
	next_index: Cell<usize>,
	//TODO:cfg[debug]
	locked: Cell<bool>,
}
impl Arena {
	pub fn new() -> Self {
		Arena {
			bytes: UnsafeCell::new(Box::new([0; 1_000_000])),
			next_index: Cell::new(0),
			locked: Cell::new(false),
		}
	}

	fn cur_index(&self) -> usize {
		self.next_index.get()
	}

	fn alloc_in_array<T: NoDrop>(&self) -> *mut T {
		self.alloc_worker()
	}

	//TODO:not pub
	pub fn alloc_single<T: NoDrop>(&self) -> *mut T {
		self.check_lock();
		self.alloc_worker()
	}

	fn check_lock(&self) {
		if self.locked.get() {
			panic!("Trying to allocate while in the middle of writing to an array.")
		}
	}

	fn alloc_worker<T: NoDrop>(&self) -> *mut T {
		let size = size_of::<T>();
		assert!(size > 0);
		self.alloc_n_bytes(size) as *mut T
	}

	fn alloc_n_bytes(&self, size: usize) -> *mut u8 {
		unsafe {
			let bytes = &mut *self.bytes.get();
			let next_index = self.next_index.get();
			let new_next_index = next_index + size;
			if new_next_index > bytes.len() {
				//Need to alloc more
				unimplemented!()
			}
			self.next_index.set(new_next_index);
			bytes.as_mut_ptr().offset(usize_to_isize(next_index))
		}
	}

	pub fn clone_slice<T: NoDrop + Copy>(&self, slice: &[T]) -> &[T] {
		//TODO:PERF faster copy
		let b = self.direct_builder::<T>();
		for em in slice {
			&b <- *em;
		}
		b.finish()
	}

	fn alloc_n<T>(&self, len: usize) -> *mut T {
		self.alloc_n_bytes(size_of::<T>() * len) as *mut T
	}

	pub fn map<T, U: NoDrop, I: KnownLen<Item = T>, F: FnMut(T) -> U>(&self, input: I, mut f: F) -> &[U] {
		//TODO:PERF don't even need to check the max size
		let len = input.len();
		let start = self.alloc_n::<U>(len);
		let mut next = start;
		unsafe {
			let max = next.offset(usize_to_isize(len));
			for x in input {
				*next = f(x);
				next = next.offset(1)
			}
			assert_eq!(next, max);
			slice::from_raw_parts(start, len)
		}
	}

	pub fn map_defined_probably_all<T, U: NoDrop, I: KnownLen<Item = T>, F: FnMut(T) -> Option<U>>(
		&self,
		input: I,
		mut f: F,
	) -> &[U] {
		let len = input.len();
		let start = self.alloc_n::<U>(len);
		let mut next = start;
		unsafe {
			let max = next.offset(usize_to_isize(len));
			for x in input {
				if let Some(u) = f(x) {
					*next = u;
					next = next.offset(1)
				}
			}
			assert!(next <= max);
			slice::from_raw_parts(start, len)
		}
	}

	pub fn exact_len_builder<T: NoDrop>(&self, len: usize) -> ExactLenBuilder<T> {
		self.check_lock();
		MaxLenBuilder::new(self.alloc_n::<T>(len), len, /*is_exact*/ true)
	}

	pub fn max_len_builder<T: NoDrop>(&self, max_len: usize) -> MaxLenBuilder<T> {
		self.check_lock();
		MaxLenBuilder::new(self.alloc_n::<T>(max_len), max_len, /*is_exact*/ false)
	}

	// Writes directly into the arena. Other allocations aren't allowed to happen at the same time.
	pub fn direct_builder<T: NoDrop>(&self) -> DirectBuilder<T> {
		self.locked.set(true);
		let start_byte_index = self.cur_index();
		DirectBuilder {
			arena: self,
			start_ptr: unsafe {
				(*self.bytes.get())
					.as_mut_ptr()
					.offset(usize_to_isize(start_byte_index)) as *mut T
			},
			start_byte_index,
		}
	}

	pub fn read_from_file(&self, mut f: File) -> IoResult<&[u8]> {
		unsafe {
			let bytes = &mut *self.bytes.get();
			let next_index = self.next_index.get();
			let buff_start = bytes.as_mut_ptr().offset(usize_to_isize(next_index));
			let capacity = bytes.len() - next_index;
			if capacity == 0 {
				unimplemented!()
			}
			let buff = slice::from_raw_parts_mut(buff_start, capacity);
			let mut buff_idx = 0;
			loop {
				let n_bytes_read = f.read(&mut buff[buff_idx..capacity])?;
				if n_bytes_read == 0 {
					break
				}
				buff_idx += n_bytes_read;
				//`- 1` to make room for the '\0' we add at the end.
				if buff_idx >= capacity - 1 {
					unimplemented!()
				}
			}
			buff[buff_idx] = b'\0';
			buff_idx += 1;
			self.next_index.set(next_index + buff_idx);
			//Also add '\0' at the end!
			Ok(&buff[0..buff_idx])
		}
	}
}
impl<'a, 'arena, T: 'a + Sized + NoDrop> Placer<T> for &'a Arena {
	type Place = PointerPlace<'a, T>;

	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.alloc_single())
	}
}

type ExactLenBuilder<'a, T> = MaxLenBuilder<'a, T>;
pub struct MaxLenBuilder<'a, T: 'a + Sized + NoDrop> {
	start: *mut T,
	max: *mut T,
	is_exact: bool,
	next: Cell<*mut T>,
	phantom: PhantomData<&'a T>,
}
impl<'a, T: 'a + Sized + NoDrop> MaxLenBuilder<'a, T> {
	fn new(start: *mut T, max_len: usize, is_exact: bool) -> Self {
		MaxLenBuilder {
			start,
			max: unsafe { start.offset(usize_to_isize(max_len)) },
			is_exact,
			next: Cell::new(start),
			phantom: PhantomData,
		}
	}

	pub fn slice_so_far(&self) -> &'a [T] {
		// unwrap() should succeed since we assert T to be non-zero in size.
		let len = isize_to_usize(self.start.offset_to(self.next.get()).unwrap());
		unsafe { slice::from_raw_parts(self.start, len) }
	}

	pub fn finish(self) -> &'a [T] {
		if self.is_exact {
			assert_eq!(self.next.get(), self.max)
		}
		self.slice_so_far()
	}
}
impl<'a, T: 'a + Sized + NoDrop + Copy> MaxLenBuilder<'a, T> {
	pub fn add_slice(&self, slice: &[T]) {
		//TODO:PERF fast copy
		for x in slice {
			self <- *x;
		}
	}
}
impl<'a, 'arena, T: 'a + Sized + NoDrop> Placer<T> for &'a MaxLenBuilder<'arena, T> {
	type Place = PointerPlace<'arena, T>;

	fn make_place(self) -> Self::Place {
		let next = self.next.get();
		let place = PointerPlace::new(next);
		let new_next = unsafe { next.offset(1) };
		assert!(new_next <= self.max);
		self.next.set(new_next);
		place
	}
}

pub struct DirectBuilder<'a, T: Sized + NoDrop> {
	arena: &'a Arena,
	start_ptr: *mut T,
	start_byte_index: usize,
}
impl<'a, T: 'a + Sized + NoDrop> DirectBuilder<'a, T> {
	pub fn finish(self) -> &'a mut [T] {
		self.arena.locked.set(false);
		let len_bytes = self.arena.cur_index() - self.start_byte_index;
		let t_size = size_of::<T>();
		assert_eq!(len_bytes % t_size, 0);
		let len = len_bytes / t_size;
		unsafe { slice::from_raw_parts_mut(self.start_ptr, len) }
	}
}
impl<'a, T: 'a + Sized + NoDrop + Copy> DirectBuilder<'a, T> {
	pub fn add_slice(&self, slice: &[T]) {
		//TODO:PERF fast copy
		for x in slice {
			self <- *x;
		}
	}
}
impl<'a, T: 'a + Sized + NoDrop> Placer<T> for &'a DirectBuilder<'a, T> {
	type Place = PointerPlace<'a, T>;

	fn make_place(self) -> Self::Place {
		PointerPlace::new(self.arena.alloc_in_array())
	}
}

/** Place for a single pointer that's already been allocated. */
pub struct PointerPlace<'a, T: 'a + Sized + NoDrop> {
	ptr: *mut T,
	phantom: PhantomData<&'a T>,
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
