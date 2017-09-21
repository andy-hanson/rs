#[macro_export]
macro_rules! builtins {
	($($name:tt |$a:ident, $b:ident, $c:ident| $value:expr,)+) => {
		Dict::of(vec!(
			$((Sym::of_str($name), Fn2(|$a, $b, $c| $value))),*
		))
	}
}
