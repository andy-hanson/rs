use util::arr::Arr;

pub enum Effect {
	Pure,
	Get,
	Set,
	Io
}

pub enum LiteralValue {
	Pass,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Real(f64),
	String(Arr<u8>),
}
