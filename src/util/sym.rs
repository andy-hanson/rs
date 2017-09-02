use std::collections::HashMap;
use std::sync::Mutex;

use util::arr::Arr;
use util::string_maker::{Show, Shower};

lazy_static! {
	static ref STRING_TO_SYMBOL: Mutex<HashMap<Box<[u8]>, Sym>> = Mutex::new(HashMap::new());
	static ref SYMBOL_TO_STRING: Mutex<HashMap<Sym, Arr<u8>>> = Mutex::new(HashMap::new());
	static ref NEXT_SYMBOL_ID: Mutex<i32> = Mutex::new(0);
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Sym(i32);

impl Sym {
	pub fn of(s: &'static str) -> Self {
		Sym::from_slice(s.as_bytes())
	}

	pub fn from_slice(input: &[u8]) -> Self {
		let mut string_to_symbol = STRING_TO_SYMBOL.lock().unwrap();
		let cached_symbol = string_to_symbol.get(input).cloned();
		cached_symbol.unwrap_or_else(|| {
			let mut next_id = NEXT_SYMBOL_ID.lock().unwrap();
			let sym = Sym(*next_id);
			*next_id += 1;
			string_to_symbol.insert(Arr::copy_from_slice(input).into_box(), sym);
			SYMBOL_TO_STRING
				.lock()
				.unwrap()
				.insert(sym, Arr::copy_from_slice(input));
			sym
		})
	}
}
impl Show for Sym {
	fn show<S: Shower>(&self, s: &mut S) {
		let map = SYMBOL_TO_STRING.lock().unwrap();
		let st = map.get(self).unwrap();
		s.add_bytes(st);
	}
}
