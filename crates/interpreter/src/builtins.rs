use util::dict::Dict;
use util::sym::Sym;

use model::module::{Module, ModuleSourceEnum};

use super::emitted_model::BuiltinCode;
use super::value::Value;

pub fn get_builtin(module: &Module, implemented: Sym) -> BuiltinCode {
	let name = match module.source {
		ModuleSourceEnum::Normal(_) =>
			// Error: only builtins can have builtin implementations
			unimplemented!(),
		ModuleSourceEnum::Builtin { name, .. } => name,
	};
	let x = PATH_TO_IMPLS.get(&name).unwrap();
	let y = x.get(&implemented).unwrap();
	(*y).clone()
}

lazy_static! {
	//TODO:PERF load each class lazily?
	static ref PATH_TO_IMPLS: Dict<Sym, Dict<Sym, BuiltinCode>> = {
		dict![
			Sym::of("Bool") => get_bool_impls(),
		]
	};
}

fn get_bool_impls() -> Dict<Sym, BuiltinCode> {
	dict![
		Sym::of("==") => BuiltinCode::Fn2(bool_eq),
	]
}

#[allow(needless_pass_by_value)]
fn bool_eq<'a>(a: Value<'a>, b: Value<'a>) -> Value<'a> {
	Value::Bool(a.as_bool() == b.as_bool())
}
