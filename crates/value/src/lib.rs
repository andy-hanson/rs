#![allow(unknown_lints)]
#![allow(new_without_default_derive)]

extern crate model;
extern crate util;

use std::marker::PhantomData;

use model::builtins::BuiltinsOwn;
use model::ty::Ty;

pub mod value_stack;
use value_stack::ValueStack;

#[derive(Clone)]
pub struct Value<'model, 'value> {
	value: ValueInner<'value>,
	//TODO:PERF cfg[debug]
	ty: &'model Ty<'model>,
}
impl<'model, 'value> Value<'model, 'value> {
	pub fn assert_void(&self, ctx: &ValueCtx<'model, 'value>) {
		self.assert_ty(&*ctx.builtins.void)
	}

	pub fn as_bool(&self, ctx: &ValueCtx<'model, 'value>) -> bool {
		self.assert_ty(&*ctx.builtins.bool);
		unsafe { self.value.bool }
	}

	pub fn as_nat(&self, ctx: &ValueCtx<'model, 'value>) -> u32 {
		self.assert_ty(&*ctx.builtins.nat);
		unsafe { self.value.nat }
	}

	pub fn as_int(&self, ctx: &ValueCtx<'model, 'value>) -> i32 {
		self.assert_ty(&*ctx.builtins.int);
		unsafe { self.value.int }
	}

	pub fn as_float(&self, ctx: &ValueCtx<'model, 'value>) -> f64 {
		self.assert_ty(&*ctx.builtins.float);
		unsafe { self.value.float }
	}

	fn assert_ty(&self, ty: &Ty<'model>) {
		assert!(self.ty.fast_equals(ty));
	}
}

pub struct ValueCtx<'model : 'value, 'value> {
	builtins: &'model BuiltinsOwn<'model>,
	heap: PhantomData<&'value Value<'model, 'value>>, //TODO: actually have a heap
}
impl<'model, 'value> ValueCtx<'model, 'value> {
	pub fn new(builtins: &'model BuiltinsOwn<'model>) -> Self {
		ValueCtx { builtins, heap: PhantomData }
	}

	pub fn new_stack(&mut self) -> ValueStack<'model, 'value> {
		ValueStack::new()
	}

	pub fn void(&self) -> Value<'model, 'value> {
		Value { value: ValueInner { void: () }, ty: &self.builtins.void }
	}

	pub fn bool(&self, bool: bool) -> Value<'model, 'value> {
		Value { value: ValueInner { bool }, ty: &self.builtins.bool }
	}

	pub fn nat(&self, nat: u32) -> Value<'model, 'value> {
		Value { value: ValueInner { nat }, ty: &self.builtins.nat }
	}

	pub fn int(&self, int: i32) -> Value<'model, 'value> {
		Value { value: ValueInner { int }, ty: &self.builtins.int }
	}

	pub fn float(&self, float: f64) -> Value<'model, 'value> {
		Value { value: ValueInner { float }, ty: &self.builtins.float }
	}
}

#[derive(Copy, Clone)]
union ValueInner<'value> {
	void: (),
	bool: bool,
	nat: u32,
	int: i32,
	float: f64,
	// Points to somewhere on the heap.
	ptr: &'value ValueInner<'value>,
}
