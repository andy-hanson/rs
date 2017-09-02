use std::rc::Rc;

use util::arr::Arr;
use util::ptr::Ptr;

use super::super::model::class::ClassDeclaration;

pub enum Value {
	Instance(Instance),
	Void,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(Rc<Arr<u8>>),
}
impl Clone for Value {
	fn clone(&self) -> Self {
		match *self {
			Value::Instance(ref i) => Value::Instance(i.clone()),
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
			unimplemented!()
		}
	}
}

#[derive(Clone)]
pub struct Instance(Rc<InstanceData>);
struct InstanceData {
	class: Ptr<ClassDeclaration>,
	slots: Arr<Value>,
}
