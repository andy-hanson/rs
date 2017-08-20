use std::cell::{ UnsafeCell };
use std::ops::Deref;

pub struct Own<T> {
	// TODO: #[cfg(debug_assertions)]
	// Does not include the owning reference, so does not deallocate anything when this reaches 0.
	ref_count: UnsafeCell<usize>,
	value: T,
}
impl<T> Own<T> {
	pub fn new(value: T) -> Own<T> {
		Own { ref_count: UnsafeCell::new(0), value }
	}

	pub fn ptr(&self) -> Ptr<T> {
		unsafe {
			*self.ref_count.get() += 1;
			Ptr(self as *const Own<T>)
		}
	}
}
impl<T> Deref for Own<T> {
	type Target = T;
	fn deref(&self) -> &T {
		&self.value
	}
}
impl<T> Drop for Own<T> {
	fn drop(&mut self) {
		unsafe {
			if *self.ref_count.get() != 0 {
				panic!("Ptr points to Own which has been dropped")
			}
		}
	}
}

pub struct Ptr<T>(*const Own<T>);
impl<T> Ptr<T> {
	pub fn clone_ptr(&self) -> Ptr<T> {
		unsafe {
			(*self.0).ptr()
		}
	}

	pub fn ptr_equals(&self, other: &Ptr<T>) -> bool {
		self.0 == other.0
	}
}
impl<T> Deref for Ptr<T> {
	type Target = T;
	fn deref(&self) -> &T {
		unsafe {
			&(*self.0).value
		}
	}
}
impl<T> Drop for Ptr<T> {
	fn drop(&mut self) {
		unsafe {
			assert!(*(*self.0).ref_count.get() != 0);
			*(*self.0).ref_count.get() -= 1
		}
	}
}

struct Late<T>(UnsafeCell<Option<T>>);
impl<T> Late<T> {
	pub fn new() -> Late<T> {
		Late(UnsafeCell::new(None))
	}

	pub fn get(&self) -> &T {
		unsafe {
			let data_ptr = self.0.get();
			(*data_ptr).as_ref().unwrap()
		}
	}

	pub fn set(&self, value: T) {
		unsafe {
			let data_ptr = self.0.get();
			assert!((*data_ptr).is_none());
			*data_ptr = Some(value)
		}
	}
}

pub struct LateOwn<T>(Late<Own<T>>);
impl<T> LateOwn<T> {
	pub fn new() -> LateOwn<T> {
		LateOwn(Late::new())
	}

	pub fn ptr(&self) -> Ptr<T> {
		self.0.get().ptr()
	}

	pub fn init(&self, value: T) {
		self.0.set(Own::new(value))
	}
}
impl<T> Deref for LateOwn<T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.0.get()
	}
}

pub struct LatePtr<T>(Late<Ptr<T>>);
impl<T> LatePtr<T> {
	pub fn new() -> LatePtr<T> {
		LatePtr(Late::new())
	}

	pub fn init(&self, value: &Own<T>) {
		self.0.set(value.ptr())
	}
}
impl<T> Deref for LatePtr<T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.0.get()
	}
}
