use compiler::model::effect::Effect;
use compiler::model::expr::LiteralValue;
use util::arr::Arr;
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::sym::Sym;

pub struct Module {
	pub imports: Arr<Import>,
	pub class: Box<Class>,
}

pub enum Import {
	Global(Loc, Path),
	Local(Loc, RelPath),
}

pub struct Class {
	pub loc: Loc,
	pub type_parameters: Arr<Sym>,
	pub head: Option<ClassHead>,
	pub supers: Arr<Super>,
	pub methods: Arr<Method>,
}

pub struct ClassHead(pub Loc, pub ClassHeadData);
pub enum ClassHeadData {
	Abstract(Arr<AbstractMethod>),
	Slots(Arr<Slot>),
	Builtin,
}

pub struct AbstractMethod {
	pub loc: Loc,
	pub type_parameters: Arr<Sym>,
	pub return_ty: Ty,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: Arr<Parameter>,
}

pub struct Slot {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty,
	pub name: Sym,
}

pub struct Super {
	pub loc: Loc,
	pub name: Sym,
	pub ty_args: Arr<Ty>,
	pub impls: Arr<Impl>,
}

pub struct Impl {
	pub loc: Loc,
	pub name: Sym,
	pub parameter_names: Arr<Sym>,
	// May be missing for a builtin
	pub body: Option<Expr>,
}

pub struct Method {
	pub loc: Loc,
	pub is_static: bool,
	pub type_parameters: Arr<Sym>,
	pub return_ty: Ty,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: Arr<Parameter>,
	pub body: Option<Expr>,
}

pub struct Parameter {
	pub loc: Loc,
	pub ty: Ty,
	pub name: Sym,
}

pub struct Ty {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: Arr<Ty>,
}

pub struct Pattern(pub Loc, pub PatternData);
impl Pattern {
	pub fn single(loc: Loc, sym: Sym) -> Pattern {
		Pattern(loc, PatternData::Single(sym))
	}
}
pub enum PatternData {
	Ignore,
	Single(Sym),
	Destruct(Arr<Pattern>),
}

pub struct Expr(pub Loc, pub ExprData);

pub enum ExprData {
	Access(Sym),
	StaticAccess(/*class_name*/ Sym, /*static_method_name*/ Sym),
	OperatorCall(Box<Expr>, Sym, Box<Expr>),
	TypeArguments(Box<Expr>, Arr<Ty>),
	Call(Box<Expr>, Arr<Expr>),
	Recur(Arr<Expr>),
	New(Arr<Ty>, Arr<Expr>),
	ArrayLiteral(Option<Ty>, Arr<Expr>),
	GetProperty(Box<Expr>, Sym),
	SetProperty(Sym, Box<Expr>),
	// This one shouldn't escape parse_expr.
	LetInProgress(Pattern, Box<Expr>),
	Let(Pattern, Box<Expr>, Box<Expr>),
	Seq(Box<Expr>, Box<Expr>),
	Literal(LiteralValue),
	SelfExpr,
	IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
	WhenTest(Arr<Case>, Box<Expr>),
	Assert(Box<Expr>),
	Try(Box<Expr>, Option<Catch>, Option<Box<Expr>>),
	For(Sym, /*looper*/ Box<Expr>, /*body*/ Box<Expr>),
}

pub struct Case(pub Loc, /*test*/ pub Expr, /*result*/ pub Expr);

pub struct Catch {
	pub loc: Loc,
	pub exception_type: Ty,
	pub exception_name_loc: Loc,
	pub exception_name: Sym,
	pub then: Box<Expr>,
}
