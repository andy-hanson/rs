use util::dict::Dict;
use util::sym::Sym;

use super::super::compiler::model::module::Module;

use super::emitted_model::{BuiltinCode};
use super::value::Value;

pub fn get_builtin(module: &Module, implemented: Sym) -> BuiltinCode {
	assert!(module.source.is_none()); // Should be a builtin module
	let x = PATH_TO_IMPLS.get(&module.name()).unwrap();
	let y = x.get(&implemented).unwrap();
	(*y).clone()
}

lazy_static! {
	//TODO:PERF load each class lazily?
	static ref PATH_TO_IMPLS: Dict<Sym, Dict<Sym, BuiltinCode>> = {
		Dict::of(vec![
			(Sym::of("Bool"), get_bool_impls()),
		])
	};
}

fn get_bool_impls() -> Dict<Sym, BuiltinCode> {
	Dict::of(vec![
		(Sym::of("=="), BuiltinCode::Fn2(bool_eq)),
	])
}

fn bool_eq(a: Value, b: Value) -> Value {
	Value::Bool(a.as_bool() == b.as_bool())
}
