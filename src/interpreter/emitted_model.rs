use util::arr::Arr;
use util::dict::Dict;
use util::ptr::{Own, Ptr};

use super::super::compiler::model::method::{Impl, MethodWithBody};

use super::value::Value;

pub struct EmittedProgram {
	pub methods: Dict<Ptr<MethodWithBody>, Own<EmittedMethod>>,
	pub impls: Dict<Ptr<Impl>, Own<EmittedImpl>>,
}

//ACTUALLY, just use one big global map of method -> implementation.
//Use regular model for the rest.
pub struct EmittedMethod {
	// Dont' worry about AbstractMethod because those don't have code
	pub source: Ptr<MethodWithBody>, // Mostly just for debug
	pub code: Code,
}

pub struct EmittedImpl {
	pub source: Ptr<Impl>,
	pub code: Code,
}

pub enum Code {
	Instructions(Instructions),
	Builtin(BuiltinCode),
}
pub struct Instructions(pub Arr<Instruction>);
#[derive(Clone)]
pub enum BuiltinCode {
	Fn0(fn() -> Value),
	Fn1(fn(&Value) -> Value),
	Fn2(fn(&Value, &Value) -> Value),
}


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
	LiteralString(Arr<u8>),
	/** Fetch a value from N values up the stack. */
	Fetch(u8),
	/**
	Pop N values out from the top value on the stack.
	Leaves the top value alone.
	*/
	UnLet(u8),
	/** Pops the `Void` value pushed by the last expression. */
	PopVoid,
}
