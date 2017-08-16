use util::arr::Arr;
use util::loc::Loc;
use util::sym::Sym;

pub mod class_declaration;
pub mod effect;
pub mod expr;
pub mod ty;
pub mod method;
pub mod module;

use self::ty::Ty;

pub enum LiteralValue {
	Pass,
	Bool(bool),
	Nat(u32),
	Int(i32),
	Real(f64),
	String(Arr<u8>),
}

pub enum Pattern {
	Ignore,
	Single(Local),
	Destruct(Loc, Arr<Pattern>),
}

pub struct Local {
	loc: Loc,
	ty: Ty,
	name: Sym,
}
impl Local {
	pub fn of(loc: Loc, ty: Ty, name: Sym) -> Local {
		Local { loc, ty, name }
	}
}
