use std::rc::Rc;

use util::arr::Arr;
use util::ptr::Ptr;

use super::super::compiler::model::class::ClassDeclaration;

pub enum Value {
	Slots(Ptr<ClassDeclaration>, Rc<Arr<Value>>),
	Void,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(Rc<Arr<u8>>),
}
impl Clone for Value {
	fn clone(&self) -> Value {
		match *self {
			Value::Slots(ref class, ref slots) => Value::Slots(class.clone_ptr(), Rc::clone(slots)),
			Value::Void => Value::Void,
			Value::Bool(b) => Value::Bool(b),
			Value::Nat(n) => Value::Nat(n),
			Value::Int(i) => Value::Int(i),
			Value::Float(f) => Value::Float(f),
			Value::String(ref s) => Value::String(Rc::clone(s)),
		}
	}
}

impl Value {
	pub fn as_bool(&self) -> bool {
		if let Value::Bool(b) = *self {
			b
		} else {
			todo!()
		}
	}
}
