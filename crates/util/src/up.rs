use std::hash::{Hash, Hasher};
use std::ops::Deref;

use serde::{Serialize, Serializer};

use super::arena::NoDrop;

pub struct Up<'a, T: 'a>(pub &'a T);
impl<'a, T: NoDrop> NoDrop for Up<'a, T> {}
impl<'a, T> Up<'a, T> {
	//TODO: shouldn't be needed, this is Copy!
	pub fn clone_as_up(&self) -> Up<'a, T> {
		Up(self.0)
	}
}
impl<'a, T> Copy for Up<'a, T> {}
#[allow(expl_impl_clone_on_copy)] // If I derive(Copy) it doesn't seem to do anything.
impl<'a, T> Clone for Up<'a, T> {
	fn clone(&self) -> Self {
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
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(self.0 as *const T as usize)
	}
}
impl<'a, T> PartialEq for Up<'a, T> {
	fn eq(&self, other: &Self) -> bool {
		ptr_eq(self.0, other.0)
	}
}

// Like Serialize, but since this is just a *reference* to the data,
// don't serialize everything, just e.g. the name.
pub trait SerializeUp {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer;
}

pub fn ptr_eq<T>(a: &T, b: &T) -> bool {
	a as *const T == b as *const T
}
