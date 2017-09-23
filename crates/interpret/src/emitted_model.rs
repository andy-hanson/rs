use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::dict::MutDict;
use util::late::Late;
use util::loc::Loc;
use util::up::Up;

use model::method::{Impl, MethodOrImpl, MethodWithBody};
use model::ty::Ty;

use value::{Value, ValueCtx};

pub struct EmittedProgram<'model: 'emit, 'emit> {
	pub methods: MethodMaps<'model, 'emit>,
}
impl<'model, 'emit> NoDrop for EmittedProgram<'model, 'emit> {}

pub struct MethodMaps<'model: 'emit, 'emit> {
	// TODO:PERF Would be nice if this stored the Code directly.
	// But that would require is to be able to store a Dict in the Arena.
	pub methods: MutDict<Up<'model, MethodWithBody<'model>>, &'emit Code<'model, 'emit>>,
	pub impls: MutDict<Up<'model, Impl<'model>>, &'emit Code<'model, 'emit>>,
}
impl<'model, 'emit> MethodMaps<'model, 'emit> {
	pub fn new() -> Self {
		MethodMaps { methods: MutDict::new(), impls: MutDict::new() }
	}

	pub fn get_method(&self, method: Up<'model, MethodWithBody<'model>>) -> &'emit Code<'model, 'emit> {
		self.methods.get(method).unwrap()
	}

	pub fn get_impl(&self, an_impl: Up<'model, Impl<'model>>) -> &'emit Code<'model, 'emit> {
		self.impls.get(an_impl).unwrap()
	}
}

pub enum Code<'model: 'emit, 'emit> {
	Instructions(Late<Instructions<'model, 'emit>>),
	Builtin(BuiltinCode),
}
impl<'model, 'emit> NoDrop for Code<'model, 'emit> {}
#[derive(Copy, Clone)]
pub enum BuiltinCode {
	Fn0(
		for<'model, 'value> fn(ctx: &ValueCtx<'model, 'value>)
			-> Value<'model, 'value>,
	),
	Fn1(
		for<'ctx, 'model, 'value> fn(Fn1Args<'ctx, 'model, 'value>)
			-> Value<'model, 'value>,
	),
	Fn2(
		for<'ctx, 'model, 'value> fn(Fn2Args<'ctx, 'model, 'value>)
			-> Value<'model, 'value>,
	),
}

pub struct Fn1Args<'ctx, 'model: 'ctx + 'value, 'value: 'ctx>(
	pub &'ctx ValueCtx<'model, 'value>,
	pub Value<'model, 'value>,
);
pub struct Fn2Args<'ctx, 'model: 'ctx + 'value, 'value: 'ctx>(
	pub &'ctx ValueCtx<'model, 'value>,
	pub Value<'model, 'value>,
	pub Value<'model, 'value>,
);

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
pub struct Instructions<'model: 'emit, 'emit>(pub &'emit [LocAndInstruction<'model, 'emit>]);
impl<'model, 'emit> NoDrop for Instructions<'model, 'emit> {}

#[derive(Copy, Clone, Serialize)]
pub struct LocAndInstruction<'model: 'emit, 'emit>(pub Loc, pub Instruction<'model, 'emit>);
impl<'model, 'emit> NoDrop for LocAndInstruction<'model, 'emit> {}

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
	Don't need to explicitly use this at the end of a method since "Return" handles that.
	*/
	UnLet(u8),
	/** Pops the `Void` value pushed by the last expression. */
	PopVoid,
	CallInstructions(CalledInstructions<'model, 'emit>),
	CallBuiltin(CalledBuiltin<'model>),
	// Pops all parameters (incl. "self") out from under the return value.
	// Then goes back to the instruction after the one that called this method.
	Return,
	// Pops N values off the stack and pushes them into slots on the heap,
	// then stores the result back on the stack.
	// TODO:PERF omit ty
	NewSlots(&'model Ty<'model>, usize), //TODO:PERF store this kind of object as its content...
	// usize is the offset in words from the start of the object.
	GetSlot(usize),
	Assert,
}
impl<'model, 'emit> NoDrop for Instruction<'model, 'emit> {}

#[derive(Copy, Clone)]
pub struct CalledInstructions<'model: 'emit, 'emit>(
	pub MethodOrImpl<'model>,
	// Must point to the Late instead of to the Instructions to avoid dereferencing a Late too early.
	pub Up<'emit, Late<Instructions<'model, 'emit>>>,
);
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
