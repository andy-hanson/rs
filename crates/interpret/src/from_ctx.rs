use value::{Value, ValueCtx};

pub trait FromCtx {
	fn from<'a, 'b>(ctx: &ValueCtx<'a, 'b>, value: Value<'a, 'b>) -> Self;
}
impl FromCtx for bool {
	fn from<'a, 'b>(ctx: &ValueCtx<'a, 'b>, value: Value<'a, 'b>) -> Self {
		value.as_bool(ctx)
	}
}
impl FromCtx for u32 {
	fn from<'a, 'b>(ctx: &ValueCtx<'a, 'b>, value: Value<'a, 'b>) -> Self {
		value.as_nat(ctx)
	}
}
impl FromCtx for i32 {
	fn from<'a, 'b>(ctx: &ValueCtx<'a, 'b>, value: Value<'a, 'b>) -> Self {
		value.as_int(ctx)
	}
}
impl FromCtx for f64 {
	fn from<'a, 'b>(ctx: &ValueCtx<'a, 'b>, value: Value<'a, 'b>) -> Self {
		value.as_float(ctx)
	}
}

pub trait ToCtx {
	fn to<'a, 'b>(self, ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b>;
}
impl ToCtx for bool {
	fn to<'a, 'b>(self, ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b> {
		ctx.bool(self)
	}
}
impl ToCtx for u32 {
	fn to<'a, 'b>(self, ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b> {
		ctx.nat(self)
	}
}
impl ToCtx for i32 {
	fn to<'a, 'b>(self, ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b> {
		ctx.int(self)
	}
}
impl ToCtx for f64 {
	fn to<'a, 'b>(self, ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b> {
		ctx.float(self)
	}
}
