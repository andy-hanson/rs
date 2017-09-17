use util::dict::Dict;
use util::sym::Sym;

use model::method::MethodOrImpl;
use model::module::{Module, ModuleSourceEnum};

use value::{ValueCtx, Value};

use super::emit::emit_error::{EmitResult, EmitError};
use super::emitted_model::BuiltinCode;

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
	static ref PATH_TO_IMPLS: Dict<Sym, Dict<Sym, BuiltinCode>> = {
		dict![
			Sym::of("Void") => get_void_impls(),
			Sym::of("Bool") => get_bool_impls(),
			Sym::of("Nat") => get_nat_impls(),
			Sym::of("Int") => get_int_impls(),
			Sym::of("Float") => get_float_impls(),
			Sym::of("String") => get_string_impls(),
		]
	};
}

fn get_void_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("value") => BuiltinCode::Fn0(void_value),
	]
}

#[allow(needless_lifetimes)]
fn void_value<'a, 'b>(ctx: &ValueCtx<'a, 'b>) -> Value<'a, 'b> {
	ctx.void()
}

fn get_nat_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("==") => BuiltinCode::Fn2(nat_eq),
	]
}

#[allow(needless_pass_by_value)]
fn nat_eq<'a, 'b>(ctx: &ValueCtx<'a, 'b>, a: Value<'a, 'b>, b: Value<'a, 'b>) -> Value<'a, 'b> {
	ctx.bool(a.as_nat(ctx) == b.as_nat(ctx))
}

fn get_bool_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("==") => BuiltinCode::Fn2(bool_eq),
	]
}

#[allow(needless_pass_by_value)]
fn bool_eq<'a, 'b>(ctx: &ValueCtx<'a, 'b>, a: Value<'a, 'b>, b: Value<'a, 'b>) -> Value<'a, 'b> {
	ctx.bool(a.as_bool(ctx) == b.as_bool(ctx))
}

fn get_int_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("==") => BuiltinCode::Fn2(int_eq),
	]
}

#[allow(needless_pass_by_value)]
fn int_eq<'a, 'b>(ctx: &ValueCtx<'a, 'b>, a: Value<'a, 'b>, b: Value<'a, 'b>) -> Value<'a, 'b> {
	ctx.bool(a.as_int(ctx) == b.as_int(ctx))
}

fn get_float_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("<") => BuiltinCode::Fn2(float_lt),
	]
}

#[allow(needless_pass_by_value)]
fn float_lt<'a, 'b>(ctx: &ValueCtx<'a, 'b>, a: Value<'a, 'b>, b: Value<'a, 'b>) -> Value<'a, 'b> {
	ctx.bool(a.as_float(ctx) < b.as_float(ctx))
}

fn get_string_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("==") => BuiltinCode::Fn2(string_eq),
	]
}

#[allow(needless_pass_by_value)]
fn string_eq<'a, 'b>(_: &ValueCtx<'a, 'b>, _: Value<'a, 'b>, _: Value<'a, 'b>) -> Value<'a, 'b> {
	unimplemented!()//Value::Bool(a.as_string()  b.as_float())
}
