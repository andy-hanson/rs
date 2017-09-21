#[macro_export]
macro_rules! dict {
	($($key:expr => $value:expr,)+) => {
		Dict::of(vec!(
			$(($key, $value)),*
		))
	}
}

#[macro_export]
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
	};
	($a: ident, $b: ident, $c: ident, $d: ident, $e: ident) => {
		let _ = $a;
		let _ = $b;
		let _ = $c;
		let _ = $d;
		let _ = $e;
	}
}
