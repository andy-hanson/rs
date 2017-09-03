use util::arr::{Arr, RcArr};
use util::dict::Dict;
use util::ptr::{Own, Ptr};

use super::super::model::method::{Impl, MethodOrImpl, MethodWithBody};

use super::value::Value;

pub struct EmittedProgram {
	pub methods: Dict<Ptr<MethodWithBody>, Own<Code>>,
	pub impls: Dict<Ptr<Impl>, Own<Code>>,
}

//ACTUALLY, just use one big global map of method -> implementation.
//Use regular model for the rest.
/*pub struct EmittedMethod {
	// Dont' worry about AbstractMethod because those don't have code
	pub source: Ptr<MethodWithBody>, // Mostly just for debug
	pub code: Code,
}*/

/*pub struct EmittedImpl {
	pub source: Ptr<Impl>,
	pub code: Code,
}*/

pub struct Code {
	pub source: MethodOrImpl,
	pub code: CodeData,
}
pub enum CodeData {
	Instructions(Own<Instructions>),
	Builtin(BuiltinCode),
}
#[derive(Clone)]
pub enum BuiltinCode {
	Fn0(fn() -> Value),
	Fn1(fn(Value) -> Value),
	Fn2(fn(Value, Value) -> Value),
}

pub struct Instructions(pub Arr<Instruction>);

/*
Note: These types are meant to be used alongside the regular model.
So they don't repeat information already contained there.
*/

// Don't need a `struct EmittedModule`, since a module just contains a set of imports and a class.
/*pub struct EmittedProgram {
	modules: Dict<Ptr<Module>, EmittedClass>,
}

// Mostly just rely on the real class.
pub struct EmittedClass {
	methods: Dict<Ptr<MethodWithBody>, EmittedMethod>,
}

pub struct EmittedMethod {
	code: Arr<Instruction>,
}*/

pub enum Instruction {
	/** Push a literal value onto the stack. */
	LiteralVoid,
	LiteralBool(bool),
	LiteralNat(u32),
	LiteralInt(i32),
	LiteralFloat(f64),
	LiteralString(RcArr<u8>),
	/** Fetch a value from N values up the stack. */
	Fetch(u8),
	/**
	Pop N values out from the top value on the stack.
	Leaves the top value alone.
	*/
	UnLet(u8),
	/** Pops the `Void` value pushed by the last expression. */
	PopVoid,
	Call(MethodOrImpl, Ptr<Instructions>),
	CallBuiltin(MethodOrImpl, BuiltinCode),
	Return,
}
