use util::dict::{Dict, MutDict};
use util::sym::Sym;

use model::method::MethodOrImpl;
use model::module::{Module, ModuleSourceEnum};

use value::{Value, ValueCtx};

use super::emit::emit_error::{EmitError, EmitResult};
use super::emitted_model::{BuiltinCode, Fn1Args, Fn2Args};
use super::from_ctx::{FromCtx, ToCtx};

pub fn get_builtin<'model>(module: &Module, method: MethodOrImpl<'model>) -> EmitResult<'model, BuiltinCode> {
	let name = match module.source {
		ModuleSourceEnum::Normal(_) =>
			// Error: only builtins can have builtin implementations
			unimplemented!(),
		ModuleSourceEnum::Builtin { name, .. } => name,
	};
	if let Some(name_to_code) = PATH_TO_IMPLS.get(name) {
		if let Some(code) = name_to_code.get(method.name()) {
			return if code.arity() == method.arity() {
				Ok(*code)
			} else {
				Err(EmitError::WrongBuiltinArity { expected: method, actual: code.arity() })
			}
		}
	}
	Err(EmitError::MissingBuiltin(method))
}

lazy_static! {
	//TODO:PERF load each class lazily?
	static ref PATH_TO_IMPLS: Dict<Sym, Dict<Sym, BuiltinCode>> = MutDict::new()
		.with_add(Sym::of(b"Void"), get_void_impls())
		.with_add(Sym::of(b"Bool"), get_bool_impls())
		.with_add(Sym::of(b"Nat"), get_nat_impls())
		.with_add(Sym::of(b"Int"), get_int_impls())
		.with_add(Sym::of(b"Float"), get_float_impls())
		.with_add(Sym::of(b"String"), get_string_impls())
		.freeze();
}

struct BuiltinsBuilder(MutDict<Sym, BuiltinCode>);
impl BuiltinsBuilder {
	fn new() -> Self {
		BuiltinsBuilder(MutDict::new())
	}

	fn fn0(mut self, name: &str, f: for<'a, 'b> fn(ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b>) -> Self {
		self.0.add(Sym::of_str(name)) <- BuiltinCode::Fn0(f);
		self
	}

	fn fn1(
		mut self,
		name: &str,
		f: for<'ctx, 'model, 'value> fn(args: Fn1Args<'ctx, 'model, 'value>)
			-> Value<'model, 'value>,
	) -> Self {
		self.0.add(Sym::of_str(name)) <- BuiltinCode::Fn1(f);
		self
	}

	fn fn2(
		mut self,
		name: &str,
		f: for<'ctx, 'model, 'value> fn(args: Fn2Args<'ctx, 'model, 'value>)
			-> Value<'model, 'value>,
	) -> Self {
		self.0.add(Sym::of_str(name)) <- BuiltinCode::Fn2(f);
		self
	}

	//fn magic<X : FromCtx, Y : FromCtx, R : ToCtx, F : Fn(X, Y) -> R>(mut self, name: &str, f: F) -> Self {
 //	self.fn2(name, |ctx, a, b| f(X::from(ctx, a), Y::from(ctx, b)).to(ctx))
 //}

	fn finish(self) -> Dict<Sym, BuiltinCode> {
		self.0.freeze()
	}
}

fn get_void_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn0("value", |ctx| ctx.void())
		.fn1("ignore", |args| args.0.void())
		.finish()
}

fn get_bool_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn2("==", |args| c2(args, |x: bool, y: bool| x == y))
		.finish()
}

fn get_nat_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn2("==", |args|
			c2(args, |x: u32, y: u32| x == y))
		.fn2("<", |args|
			c2(args, |x: u32, y: u32| x < y))
		.fn2(">", |args|
			c2(args, |x: u32, y: u32| x > y))
		.fn2("<=", |args|
			c2(args, |x: u32, y: u32| x <= y))
		.fn2(">=", |args|
			c2(args, |x: u32, y: u32| x >= y))
		.fn2("+", |args|
			c2(args, |x: u32, y: u32| x + y)) //TODO: overflow -> noze exception
		.fn2("-", |args|
			c2(args, |x: u32, y: u32| x - y)) //TODO: overflow -> noze exception (large nat - 0 won't fit in `Int`)
		.fn2("*", |args|
			c2(args, |x: u32, y: u32| x * y)) //TODO: overflow -> noze exception
		.fn2("/", |args|
			c2(args, |x: u32, y: u32| x / y)) //TODO: /0 -> noze exception
		.finish()
}

fn get_int_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn2("==", |args|
			c2(args, |x: i32, y: i32| x == y))
		.fn2("<", |args|
			c2(args, |x: i32, y: i32| x < y))
		.fn2(">", |args|
			c2(args, |x: i32, y: i32| x > y))
		.fn2("<=", |args|
			c2(args, |x: i32, y: i32| x <= y))
		.fn2(">=", |args|
			c2(args, |x: i32, y: i32| x >= y))
		.fn2("+", |args|
			c2(args, |x: i32, y: i32| x + y)) //TODO: overflow -> noze exception
		.fn2("-", |args|
			c2(args, |x: i32, y: i32| x - y)) //TODO: overflow -> noze exception (large nat - 0 won't fit in `Int`)
		.fn2("*", |args|
			c2(args, |x: i32, y: i32| x * y)) //TODO: overflow -> noze exception
		.fn2("/", |args|
			c2(args, |x: i32, y: i32| x / y)) //TODO: /0 -> noze exception
		.finish()
}

fn get_float_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn2("<", |args| c2(args, |x: f64, y: f64| x < y))
		.fn2(">", |args| c2(args, |x: f64, y: f64| x > y))
		.fn2("<=", |args| c2(args, |x: f64, y: f64| x > y))
		.fn2(">=", |args| c2(args, |x: f64, y: f64| x > y))
		.fn2("+", |args| c2(args, |x: f64, y: f64| x + y))
		.fn2("-", |args| c2(args, |x: f64, y: f64| x - y))
		.fn2("*", |args| c2(args, |x: f64, y: f64| x * y))
		.fn2("/", |args| c2(args, |x: f64, y: f64| x / y))
		.finish()
}

fn get_string_impls() -> Dict<Sym, BuiltinCode> {
	BuiltinsBuilder::new()
		.fn2("==", |_args| unimplemented!())
		.finish()
}


fn c1<'ctx, 'a, 'b, X: FromCtx, R: ToCtx, F: Fn(X) -> R>(
	Fn1Args(ctx, a): Fn1Args<'ctx, 'a, 'b>,
	f: F,
) -> Value<'a, 'b> {
	f(X::from(ctx, a)).to(ctx)
}

fn c2<'ctx, 'a, 'b, X: FromCtx, Y: FromCtx, R: ToCtx, F: Fn(X, Y) -> R>(
	Fn2Args(ctx, a, b): Fn2Args<'ctx, 'a, 'b>,
	f: F,
) -> Value<'a, 'b> {
	f(X::from(ctx, a), Y::from(ctx, b)).to(ctx)
}
