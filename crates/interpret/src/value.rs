use std::rc::Rc;

use model::class::ClassDeclaration;

pub enum Value<'model> {
	Instance(Instance<'model>),
	Void,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Float(f64),
	//String(), TODO: this must be garbage-collected data
}
impl<'model> Clone for Value<'model> {
	fn clone(&self) -> Self {
		match *self {
			Value::Instance(ref i) => Value::Instance(i.clone()),
			Value::Void => Value::Void,
			Value::Bool(b) => Value::Bool(b),
			Value::Nat(n) => Value::Nat(n),
			Value::Int(i) => Value::Int(i),
			Value::Float(f) => Value::Float(f),
			//Value::String(ref s) => Value::String(RcArr::clone(s)),
		}
	}
}
impl<'model> Value<'model> {
	pub fn is_void(&self) -> bool {
		if let Value::Void = *self {
			true
		} else {
			false
		}
	}

	pub fn as_bool(&self) -> bool {
		if let Value::Bool(b) = *self {
			b
		} else {
			unreachable!()
		}
	}

	pub fn as_nat(&self) -> u32 {
		if let Value::Nat(n) = *self {
			n
		} else {
			unreachable!()
		}
	}

	pub fn as_int(&self) -> i32 {
		if let Value::Int(i) = *self {
			i
		} else {
			unreachable!()
		}
	}

	pub fn as_float(&self) -> f64 {
		if let Value::Float(f) = *self {
			f
		} else {
			unreachable!()
		}
	}
}

#[derive(Clone)]
pub struct Instance<'model>(Rc<InstanceData<'model>>);
struct InstanceData<'model> {
	class: &'model ClassDeclaration<'model>,
	slots: [Value<'model>],
}