use std::str::FromStr;

pub trait U8SliceOps {
	fn __self(&self) -> &[u8];

	fn clone_to_utf8_string(&self) -> String {
		//TODO:PERF
		let x = ::std::str::from_utf8(self.__self()).unwrap();
		String::from_str(x).unwrap()
	}

	fn equals_str(&self, s: &str) -> bool {
		unused!(s);
		//TODO:PERF
		//self.each_equals(&Arr::copy_from_str(s), |a, b| a == b)
		unimplemented!()
	}

	fn without_end_if_ends_with(&self, end: &[u8]) -> &[u8] {
		let slice = self.__self();
		let (offset, overflow) = slice.len().overflowing_sub(end.len());
		if overflow {
			return slice
		}

		for (i, ch) in end.iter().enumerate() {
			if slice[offset + i] != *ch {
				return slice
			}
		}

		&slice[0..offset]
	}
}
impl U8SliceOps for [u8] {
	fn __self(&self) -> &[u8] {
		self
	}
}
