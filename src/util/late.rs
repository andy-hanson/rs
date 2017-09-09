use serde::{Serialize, Serializer};

use std::cell::UnsafeCell;
use std::ops::Deref;

use super::arena::NoDrop;

pub struct Late<T>(UnsafeCell<Option<T>>); //TODO:UnsafeCell
impl<T> Late<T> {
	pub fn new() -> Late<T> {
		Late(UnsafeCell::new(None))
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

	pub fn init(&self, value: T) -> &T {
		unsafe {
			let data_ptr = self.0.get();
			assert!((*data_ptr).is_none());
			*data_ptr = Some(value)
		}
		&*self
	}
}
impl<'a, T : NoDrop> NoDrop for Late<&'a T> {}
impl<T> Deref for Late<T> {
	type Target = T;

	fn deref(&self) -> &T {
		self.try_get().unwrap()
	}
}
impl<T : Serialize> Serialize for Late<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		unsafe { self.0.get().as_ref().unwrap().serialize(serializer) }
	}
}
