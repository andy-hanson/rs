use serde::{Serialize, Serializer};

use std::cell::{Cell, UnsafeCell};
use std::mem::uninitialized;
use std::ops::{Deref, InPlace, Place, Placer};

use super::arena::NoDrop;

// Important that T : NoDrop because we don't want the Late to have a Drop impl,
// since it might contain uninitialized memory.
pub struct Late<T: NoDrop> {
	initialized: Cell<bool>, //TODO: cfg[debug]
	value: UnsafeCell<T>,
}
impl<T: NoDrop> Late<T> {
	pub fn new() -> Late<T> {
		Late { initialized: Cell::new(false), value: UnsafeCell::new(unsafe { uninitialized() }) }
	}

	pub fn into_value(self) -> T {
		assert!(self.initialized.get());
		unsafe { self.value.into_inner() }
	}

	pub fn try_get(&self) -> Option<&T> {
		if self.initialized.get() {
			Some(unsafe { self.value.get().as_ref().unwrap() })
		} else {
			None
		}
	}

	#[allow(mut_from_ref)]
	pub fn unsafe_get_mut(&self) -> &mut T {
		assert!(self.initialized.get());
		unsafe { self.value.get().as_mut().unwrap() }
	}

	// Don't do this unless you know what you're doing...
	pub fn initialize_or_overwrite(&self, value: T) {
		self.initialized.set(true);
		unsafe { *self.value.get() = value }
	}

	pub fn try_get_mut(&mut self) -> Option<&mut T> {
		if self.initialized.get() {
			Some(unsafe { self.value.get().as_mut().unwrap() })
		} else {
			None
		}
	}
}
impl<'a, T: NoDrop> NoDrop for Late<&'a T> {}
impl<T: NoDrop> Deref for Late<T> {
	type Target = T;

	fn deref(&self) -> &T {
		self.try_get().unwrap()
	}
}
impl<T: NoDrop + Serialize> Serialize for Late<T> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.deref().serialize(serializer)
	}
}
impl<'a, T: NoDrop> Placer<T> for &'a Late<T> {
	type Place = Self;

	fn make_place(self) -> Self::Place {
		self
	}
}
impl<'a, T: NoDrop> Place<T> for &'a Late<T> {
	fn pointer(&mut self) -> *mut T {
		assert!(!self.initialized.get());
		self.value.get()
	}
}
impl<'a, T: NoDrop> InPlace<T> for &'a Late<T> {
	type Owner = &'a T;

	unsafe fn finalize(self) -> Self::Owner {
		self.initialized.set(true);
		self.deref()
	}
}
