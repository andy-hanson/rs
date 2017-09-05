use std::borrow::Borrow;
use std::cell::{Cell};
use std::hash::{Hash, Hasher};
use std::intrinsics::type_name;
use std::ops::Deref;

use serde::{Serialize, Serializer};

use std::sync::Mutex;

use super::late::Late;

//TODO:KILL
lazy_static! {
	static ref NEXT_OWN_ID: Mutex<usize> = Mutex::new(100);
	static ref NEXT_PTR_ID: Mutex<usize> = Mutex::new(200);
}

fn get_and_incr(m: &Mutex<usize>) -> usize {
	let mut next_id = m.lock().unwrap();
	let id = *next_id;
	*next_id += 1;
	id
}

pub struct Own<T> {
	//TODO:KILL
	id: usize,
	// TODO: #[cfg(debug_assertions)]
	// Does not include the owning reference, so does not deallocate anything when this reaches 0.
	//TODO:UnsafeCell
	ref_count: Cell<usize>,
	value: T,
}
impl<T> Own<T> {
	pub fn new(value: T) -> Self {
		let id = get_and_incr(&NEXT_OWN_ID);
		let o = Own { id, ref_count: Cell::new(0), value };
		println!("{}: Created", o.desc());
		o
	}

	pub fn ptr(&self) -> Ptr<T> {
		//*self.ref_count.as_ptr() += 1;
		self.ref_count.set(self.ref_count.get() + 1);
		Ptr::new(self as *const Own<T>)
	}

	fn decr_ref_count(&self) {
		println!("{}: Dropping a Ptr", self.desc());
		if self.ref_count.get() == 0 {
			let msg = format!("{}: Dropping a Ptr, but ref_count is already 0", self.desc());
			println!("{}", msg);
			panic!(msg)
		}
		self.ref_count.set(self.ref_count.get() - 1)
	}

	fn desc(&self) -> String {
		format!("Own<{}>(id={}, ref_count={})", unsafe { type_name::<T>() }, self.id, self.ref_count.get())
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
		if self.ref_count.get() != 0 {
			let msg = format!("{}: Trying to drop, but there is some Ptr to it", self.desc());
			println!("{}", msg);
			panic!(msg)
		}
		println!("{}: Dropped", self.desc());
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
impl<T: Serialize> Serialize for Own<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.value.serialize(serializer)
	}
}

pub struct Ptr<T> {
	id: usize,
	value: *const Own<T>,
}
impl<T> Ptr<T> {
	fn new(value: *const Own<T>) -> Self {
		let id = get_and_incr(&NEXT_PTR_ID);
		unsafe {
			println!("{}: Attained a Ptr {}", (*value).desc(), id);
		}
		Ptr { id, value }
	}

	pub fn clone_ptr(&self) -> Ptr<T> {
		unsafe { (*self.value).ptr() }
	}

	pub fn ptr_equals(&self, other: &Ptr<T>) -> bool {
		self.value == other.value
	}
}
impl<T> Deref for Ptr<T> {
	type Target = T;
	fn deref(&self) -> &T {
		unsafe { &(*self.value).value }
	}
}
impl<T> Drop for Ptr<T> {
	fn drop(&mut self) {
		unsafe {
			println!("Drop ptr {}", self.id);
			(*self.value).decr_ref_count();
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
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		hasher.write_usize(self.value as usize)
	}
}
impl<T: SerializeAsPtr> Serialize for Ptr<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		(*self).serialize_as_ptr(serializer)
	}
}

// Like Serialize, but since this is jus ta *reference* to the data,
// don't serialize everything, just e.g. the name.
pub trait SerializeAsPtr {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer;
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
impl<T: Serialize> Serialize for LateOwn<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.deref().serialize(serializer)
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
