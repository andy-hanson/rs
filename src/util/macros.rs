#[macro_export]
macro_rules! todo {
	() => {
		panic!()
	};
}

macro_rules! arr {
	($($e:expr),*) => {
		Arr::from_vec(vec!($($e),*))
	}
}

macro_rules! unwrap_or_return {
	($option: expr, $or: expr) => {
		match $option {
			Some(x) => x,
			None => return $or
		}
	};
}

#[macro_export]
macro_rules! unused {
	($a: ident) => {
		let _ = $a;
	};
	($a: ident, $b: ident) => {
		let _ = $a;
		let _ = $b;
	};
	($a: ident, $b: ident, $c: ident) => {
		let _ = $a;
		let _ = $b;
		let _ = $c;
	};
	($a: ident, $b: ident, $c: ident, $d: ident) => {
		let _ = $a;
		let _ = $b;
		let _ = $c;
		let _ = $d;
	}
}
