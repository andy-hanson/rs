use std::collections::HashMap;
use std::sync::Mutex;

use util::arr::Arr;

lazy_static! {
    static ref STRING_TO_SYMBOL: Mutex<HashMap<Arr<u8>, Sym>> = Mutex::new(HashMap::new());
	static ref SYMBOL_TO_STRING: Mutex<HashMap<Sym, String>> = Mutex::new(HashMap::new());
	static ref NEXT_SYMBOL_ID: Mutex<i32> = Mutex::new(0);
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Sym(i32);

impl Sym {
	pub fn from_arr(input_arr: Arr<u8>) -> Sym {
		let mut string_to_symbol = STRING_TO_SYMBOL.lock().unwrap();
		// Need to clone the contents to prevent lifetime errors.
		let cached_symbol: Option<Sym> = string_to_symbol.get(&input_arr).map(|x| x.clone());
		match cached_symbol {
			Some(s) => s,
			None => {
				let mut next_id = NEXT_SYMBOL_ID.lock().unwrap();
				let sym = Sym(*next_id);
				*next_id += 1;
				let string = input_arr.clone_to_utf8_string();
				string_to_symbol.insert(input_arr, sym);
				let mut symbol_to_string = SYMBOL_TO_STRING.lock().unwrap();
				symbol_to_string.insert(sym, string);
				return sym;
			}
		}
	}

	pub fn to_str(&self) -> String {
		let map = SYMBOL_TO_STRING.lock().unwrap();
		let s = map.get(self).unwrap();
		s.clone()
	}
}