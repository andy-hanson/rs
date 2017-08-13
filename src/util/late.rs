use std::cell::RefCell;
use std::rc::{ Rc, Weak };

pub struct Late<T>(RefCell<Weak<T>>);
impl<T> Late<T> {
	pub fn new() -> Late<T> {
		Late(RefCell::new(Weak::new()))
	}

	pub fn get(&self) -> Rc<T> {
		self.0.borrow().upgrade().unwrap()
	}

	pub fn set(&self, value: &Rc<T>) {
		*self.0.borrow_mut() = Rc::downgrade(&value);
	}
}
