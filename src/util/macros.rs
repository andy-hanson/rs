#[macro_export]
macro_rules! todo {
	() => {
		panic!()
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
