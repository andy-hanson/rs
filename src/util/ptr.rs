use std::borrow::Borrow;
use std::cell::UnsafeCell;
use std::hash::{Hash, Hasher};
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
impl<T> Borrow<T> for Own<T> {
	fn borrow(&self) -> &T {
		&*self
	}
}
impl<T: PartialEq> PartialEq for Own<T> {
	fn eq(&self, other: &Own<T>) -> bool {
		let self_inner: &T = self.borrow();
		let other_inner: &T = other.borrow();
		self_inner == other_inner
	}
}
impl<T: Eq> Eq for Own<T> {}
impl<T: Hash> Hash for Own<T> {
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		let inner: &T = self.borrow();
		inner.hash(hasher)
	}
}

pub struct Ptr<T>(*const Own<T>);
impl<T> Ptr<T> {
	pub fn clone_ptr(&self) -> Ptr<T> {
		unsafe { (*self.0).ptr() }
	}

	pub fn ptr_equals(&self, other: &Ptr<T>) -> bool {
		self.0 == other.0
	}
}
impl<T> Deref for Ptr<T> {
	type Target = T;
	fn deref(&self) -> &T {
		unsafe { &(*self.0).value }
	}
}
impl<T> Drop for Ptr<T> {
	fn drop(&mut self) {
		unsafe {
			assert_eq!(*(*self.0).ref_count.get(), 0);
			*(*self.0).ref_count.get() -= 1
		}
	}
}
impl<T> Eq for Ptr<T> {}
impl<T> PartialEq for Ptr<T> {
	fn eq(&self, other: &Ptr<T>) -> bool {
		self.ptr_equals(other)
	}
}
impl<T> Hash for Ptr<T> {
	fn hash<H : Hasher>(&self, hasher: &mut H) {
		hasher.write_usize(self.0 as usize)
	}
}

pub struct Late<T>(UnsafeCell<Option<T>>);
impl<T> Late<T> {
	pub fn new() -> Late<T> {
		Late(UnsafeCell::new(None))
	}

	pub fn get(&self) -> &T {
		self.try_get().unwrap()
	}

	pub fn try_get(&self) -> Option<&T> {
		unsafe {
			let data_ptr = self.0.get();
			(*data_ptr).as_ref()
		}
	}

	pub fn into_value(self) -> T {
		unsafe { self.0.into_inner().unwrap() }
	}

	pub fn init(&self, value: T) {
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
		self.0.init(Own::new(value))
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

	pub fn init(&self, value: Ptr<T>) {
		self.0.init(value)
	}

	pub fn into_ptr(self) -> Ptr<T> {
		self.0.into_value()
	}

	pub fn try_get(&self) -> Option<&T> {
		self.0.try_get().map(|ptr| &**ptr)
	}

	pub fn try_get_ptr(&self) -> Option<Ptr<T>> {
		self.0.try_get().map(|ptr| ptr.clone_ptr())
	}
}
impl<T> Deref for LatePtr<T> {
	type Target = T;
	fn deref(&self) -> &T {
		self.0.get()
	}
}
