use std::marker::PhantomData;

use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::dict::MutDict;
use util::late::Late;
use util::up::Up;

use model::method::{Impl, MethodOrImpl, MethodWithBody};

use super::value::Value;

pub struct EmittedProgram<'model: 'emit, 'emit> {
	pub methods: MethodMaps<'model, 'emit>,
}
impl<'model, 'emit> NoDrop for EmittedProgram<'model, 'emit> {}

pub struct MethodMaps<'model : 'emit, 'emit> {
	//TODO:PERF Would be nice if this stored the Code directly. But that would require is to be able to store a Dict in the Arena.
	pub methods: MutDict<Up<'model, MethodWithBody<'model>>, &'emit Code<'model, 'emit>>,
	pub impls: MutDict<Up<'model, Impl<'model>>, &'emit Code<'model, 'emit>>,
}
impl<'model, 'emit> MethodMaps<'model, 'emit> {
	pub fn new() -> Self {
		MethodMaps { methods: MutDict::new(), impls: MutDict::new() }
	}

	pub fn get_method(&self, method: Up<'model, MethodWithBody<'model>>) -> &'emit Code<'model, 'emit> {
		self.methods.get(&method).unwrap()
	}

	pub fn get_impl(&self, an_impl: Up<'model, Impl<'model>>) -> &'emit Code<'model, 'emit> {
		self.impls.get(&an_impl).unwrap()
	}
}

pub enum Code<'model: 'emit, 'emit> {
	Instructions(Late<Instructions<'model, 'emit>>),
	Builtin(BuiltinCode),
}
impl<'model, 'emit> NoDrop for Code<'model, 'emit> {}
#[derive(Copy, Clone)]
pub enum BuiltinCode {
	Fn0(for<'a> fn(dummy: PhantomData<&'a ()>) -> Value<'a>),
	Fn1(for<'a> fn(Value<'a>) -> Value<'a>),
	Fn2(for<'a> fn(Value<'a>, Value<'a>) -> Value<'a>),
}
impl BuiltinCode {
	pub fn arity(self) -> u8 {
		match self {
			BuiltinCode::Fn0(_) => 0,
			BuiltinCode::Fn1(_) => 1,
			BuiltinCode::Fn2(_) => 2,
		}
	}
}

#[derive(Serialize)]
pub struct Instructions<'model: 'emit, 'emit>(pub &'emit [Instruction<'model, 'emit>]);
impl<'model, 'emit> NoDrop for Instructions<'model, 'emit> {}

#[derive(Copy, Clone, Serialize)]
pub enum Instruction<'model: 'emit, 'emit> {
	LiteralNat(u32),
	LiteralInt(i32),
	LiteralFloat(f64),
	LiteralString(&'model [u8]),
	/** Fetch a value from N values up the stack. */
	Fetch(u8),
	/**
	Pop N values out from the top value on the stack.
	Leaves the top value alone.
	*/
	UnLet(u8),
	/** Pops the `Void` value pushed by the last expression. */
	PopVoid,
	CallInstructions(CalledInstructions<'model, 'emit>),
	CallBuiltin(CalledBuiltin<'model>),
	Return,
}
impl<'model, 'emit> NoDrop for Instruction<'model, 'emit> {}

#[derive(Copy, Clone)]
pub struct CalledInstructions<'model : 'emit, 'emit>(pub MethodOrImpl<'model>, pub Up<'emit, Instructions<'model, 'emit>>);
impl<'model, 'emit> Serialize for CalledInstructions<'model, 'emit> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.0.serialize(serializer)
	}
}

#[derive(Copy, Clone)]
pub struct CalledBuiltin<'model>(pub MethodOrImpl<'model>, pub BuiltinCode);
impl<'model> Serialize for CalledBuiltin<'model> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.0.serialize(serializer)
	}
}

