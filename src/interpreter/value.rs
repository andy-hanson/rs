use std::rc::Rc;

use util::arr::Arr;
use util::ptr::Ptr;

use super::super::compiler::model::class::ClassDeclaration;

pub enum Value {
	Slots(Ptr<ClassDeclaration>, Arr<Rc<Value>>),
	Void,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	String(Arr<u8>),
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