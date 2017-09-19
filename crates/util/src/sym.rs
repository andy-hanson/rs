use serde::{Serialize, Serializer};

use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result as FormatResult};
use std::ops::Deref;
use std::sync::Mutex;

use super::arena::NoDrop;
use super::show::{Show, Shower};
use super::u8_slice_ops::U8SliceOps;

lazy_static! {
	//TODO:PERF compare perf to https://github.com/kinghajj/shawshank
	static ref STRING_TO_SYMBOL: Mutex<HashMap<Box<[u8]>, Sym>> = Mutex::new(HashMap::new());
	static ref SYMBOL_TO_STRING: Mutex<HashMap<Sym, Box<[u8]>>> = Mutex::new(HashMap::new());
	static ref NEXT_SYMBOL_ID: Mutex<u32> = Mutex::new(0);
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Sym(u32);
impl NoDrop for Sym {}

impl Sym {
	pub fn of(input: &[u8]) -> Self {
		let mut string_to_symbol = STRING_TO_SYMBOL.lock().unwrap();
		let cached_symbol = string_to_symbol.get(input).cloned();
		cached_symbol.unwrap_or_else(|| {
			let mut next_id = NEXT_SYMBOL_ID.lock().unwrap();
			let sym = Sym(*next_id);
			*next_id += 1;
			string_to_symbol.insert(input.to_owned().into_boxed_slice(), sym);
			SYMBOL_TO_STRING
				.lock()
				.unwrap()
				.insert(sym, input.to_owned().into_boxed_slice());
			sym
		})
	}
}
impl Show for Sym {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		let map = SYMBOL_TO_STRING.lock().unwrap();
		let str = map.get(&self).unwrap().deref();
		s.add(str)?;
		Ok(())
	}
}
impl Serialize for Sym {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let map = SYMBOL_TO_STRING.lock().unwrap();
		let str = map.get(self).unwrap(); // TODO: duplicate code...
		serializer.serialize_str(&str.clone_to_utf8_string())
	}
}
impl Debug for Sym {
	fn fmt(&self, f: &mut Formatter) -> FormatResult {
		let map = SYMBOL_TO_STRING.lock().unwrap();
		let str = map.get(self).unwrap().deref(); // TODO: duplicate code...
		f.write_str(&String::from_utf8_lossy(str))
	}
}
//TODO:cfg[debug]
