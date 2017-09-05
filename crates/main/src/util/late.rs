use std::cell::UnsafeCell;

use super::arena::NoDrop;

pub struct Late<T>(UnsafeCell<Option<T>>); //TODO:UnsafeCell
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
		self.into_option().unwrap()
	}

	// Like `into_value`, but returns `None` if this was never initialized.
	pub fn into_option(self) -> Option<T> {
		unsafe { self.0.into_inner() }
	}

	pub fn init(&self, value: T) {
		unsafe {
			let data_ptr = self.0.get();
			assert!((*data_ptr).is_none());
			*data_ptr = Some(value)
		}
	}
}
impl<'a, T> NoDrop for Late<&'a T> where T : NoDrop {
	const NO_DROP_MARKER: u8 = T::NO_DROP_MARKER;
}
