use util::dict::Dict;
use util::sym::Sym;

use model::method::MethodOrImpl;
use model::module::{Module, ModuleSourceEnum};

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
			Sym::of(b"Void") => get_void_impls(),
			Sym::of(b"Bool") => get_bool_impls(),
			Sym::of(b"Nat") => get_nat_impls(),
			Sym::of(b"Int") => get_int_impls(),
			Sym::of(b"Float") => get_float_impls(),
			Sym::of(b"String") => get_string_impls(),
		]
	};
}

fn get_void_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"value") => BuiltinCode::Fn0(|ctx|
			ctx.void()),
	]
}

fn get_nat_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"==") => BuiltinCode::Fn2(|ctx, a, b|
			ctx.bool(a.as_nat(ctx) == b.as_nat(ctx))),
	]
}

fn get_bool_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"==") => BuiltinCode::Fn2(|ctx, a, b|
			ctx.bool(a.as_bool(ctx) == b.as_bool(ctx))),
	]
}

fn get_int_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"==") => BuiltinCode::Fn2(|ctx, a, b|
			ctx.bool(a.as_int(ctx) == b.as_int(ctx))),
	]
}

fn get_float_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"<") => BuiltinCode::Fn2(|ctx, a, b|
			ctx.bool(a.as_float(ctx) < b.as_float(ctx))),
	]
}

fn get_string_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of(b"==") => BuiltinCode::Fn2(|_ctx, _a, _b|
			unimplemented!()),
	]
}
