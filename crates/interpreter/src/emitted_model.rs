use std::marker::PhantomData;

use util::arena::{NoDrop, Up};
use util::dict::Dict;

use model::method::{Impl, MethodOrImpl, MethodWithBody};

use super::value::Value;

pub struct EmittedProgram<'model: 'emit, 'emit> {
	pub methods: Dict<Up<'model, MethodWithBody<'model>>, Code<'model, 'emit>>,
	pub impls: Dict<Up<'model, Impl<'model>>, Code<'model, 'emit>>,
}
impl<'model, 'emit> NoDrop for EmittedProgram<'model, 'emit> {}

pub struct Code<'model: 'emit, 'emit> {
	pub source: MethodOrImpl<'model>,
	pub code: CodeData<'model, 'emit>,
}
pub enum CodeData<'model: 'emit, 'emit> {
	Instructions(Instructions<'model, 'emit>),
	Builtin(BuiltinCode),
}
#[derive(Clone)]
pub enum BuiltinCode {
	Fn0(for<'a> fn(dummy: PhantomData<&'a ()>) -> Value<'a>),
	Fn1(for<'a> fn(Value<'a>) -> Value<'a>),
	Fn2(for<'a> fn(Value<'a>, Value<'a>) -> Value<'a>),
}

pub struct Instructions<'model: 'emit, 'emit>(pub &'emit [Instruction<'model, 'emit>]);
impl<'model, 'emit> NoDrop for Instructions<'model, 'emit> {}

pub enum Instruction<'model: 'emit, 'emit> {
	/** Push a literal value onto the stack. */
	LiteralVoid,
	LiteralBool(bool),
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
	Call(MethodOrImpl<'model>, Up<'emit, Instructions<'model, 'emit>>),
	CallBuiltin(MethodOrImpl<'model>, BuiltinCode),
	Return,
}
impl<'model, 'emit> NoDrop for Instruction<'model, 'emit> {}
