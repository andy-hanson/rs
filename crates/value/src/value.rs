use util::arena::NoDrop;

use model::ty::Ty;

use super::ctx::ValueCtx;

#[derive(Copy, Clone)]
pub struct Value<'model : 'value, 'value> {
	pub(crate) value: ValueInner<'model, 'value>,
	//TODO:PERF cfg[debug]
	pub(crate) ty: &'model Ty<'model>,
}
impl<'model, 'value> NoDrop for Value<'model, 'value> {}
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

#[derive(Copy, Clone)]
pub(crate) union ValueInner<'model : 'value, 'value> {
	pub void: (),
	pub bool: bool,
	pub nat: u32,
	pub int: i32,
	pub float: f64,
	// Points to somewhere on the heap.
	// Note that this isn't just a pointer to one value, but to however many values are necessary.
	// TODO:PERF but still use a raw pointer and not a slice!
	pub ptr: &'value [Value<'model, 'value>],
}
impl<'model, 'value> NoDrop for ValueInner<'model, 'value> {}
