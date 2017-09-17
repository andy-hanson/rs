#[macro_use]
extern crate serde_derive;

extern crate util;

use util::arena::NoDrop;
use util::iter::slice_is_empty;
use util::late::Late;
use util::list::List;
use util::loc::Loc;
use util::path::{Path, RelPath};
use util::sym::Sym;

// Our own copy of model::effect to avoid dependency
#[derive(Copy, Clone, Eq, PartialEq, Serialize)]
pub enum Effect { Pure, Get, Set, Io }

#[derive(Serialize)]
pub struct Module<'a> {
	pub imports: List<'a, Import<'a>>,
	pub class: &'a Class<'a>,
}
impl<'a> NoDrop for Module<'a> {}

#[derive(Serialize)]
pub enum Import<'a> {
	Global(Loc, Path<'a>),
	Local(Loc, RelPath<'a>),
}
impl<'a> NoDrop for Import<'a> {}

#[derive(Serialize)]
pub struct Class<'a> {
	pub loc: Loc,
	#[serde(skip_serializing_if = "slice_is_empty")]
	pub type_parameters: &'a [Sym],
	pub head: Option<ClassHead<'a>>,
	#[serde(skip_serializing_if = "List::is_empty_ref")]
	pub supers: List<'a, Super<'a>>,
	pub methods: List<'a, Method<'a>>,
}
impl<'a> NoDrop for Class<'a> {}

#[derive(Serialize)]
pub struct ClassHead<'a>(pub Loc, pub ClassHeadData<'a>);
impl<'a> NoDrop for ClassHead<'a> {}
#[derive(Serialize)]
pub enum ClassHeadData<'a> {
	Abstract(List<'a, AbstractMethod<'a>>),
	Slots(List<'a, Slot<'a>>),
	Builtin,
}
impl<'a> NoDrop for ClassHeadData<'a> {}

#[derive(Serialize)]
pub struct AbstractMethod<'a> {
	pub loc: Loc,
	#[serde(skip_serializing_if = "slice_is_empty")]
	pub type_parameters: &'a [Sym],
	pub return_ty: Ty<'a>,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: List<'a, Parameter<'a>>,
}
impl<'a> NoDrop for AbstractMethod<'a> {}

#[derive(Serialize)]
pub struct Slot<'a> {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Slot<'a> {}

#[derive(Serialize)]
pub struct Super<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
	pub impls: List<'a, Impl<'a>>,
}
impl<'a> NoDrop for Super<'a> {}

#[derive(Serialize)]
pub struct Impl<'a> {
	pub loc: Loc,
	pub name: Sym,
	pub parameter_names: &'a [Sym],
	// May be missing for a builtin
	pub body: Option<Expr<'a>>,
}
impl<'a> NoDrop for Impl<'a> {}

#[derive(Serialize)]
pub struct Method<'a> {
	pub loc: Loc,
	pub is_static: bool,
	#[serde(skip_serializing_if = "slice_is_empty")]
	pub type_parameters: &'a [Sym],
	pub return_ty: Ty<'a>,
	pub name: Sym,
	pub self_effect: Effect,
	pub parameters: List<'a, Parameter<'a>>,
	pub body: Option<Expr<'a>>,
}
impl<'a> NoDrop for Method<'a> {}

#[derive(Serialize)]
pub struct Parameter<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Parameter<'a> {}

#[derive(Serialize)]
pub struct Ty<'a> {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: List<'a, Ty<'a>>,
}
impl<'a> NoDrop for Ty<'a> {}

#[derive(Serialize)]
pub struct Pattern<'a>(pub Loc, pub PatternData<'a>);
impl<'a> NoDrop for Pattern<'a> {}
#[derive(Serialize)]
pub enum PatternData<'a> {
	Ignore,
	Single(Sym),
	Destruct(&'a [Pattern<'a>]),
}
impl<'a> NoDrop for PatternData<'a> {}

#[derive(Serialize)]
pub struct Expr<'a> {
	pub loc: Loc,
	pub data: ExprData<'a>,
}
impl<'a> NoDrop for Expr<'a> {}

// Make sure every variant takes less than one word in size!
#[derive(Serialize)]
pub enum ExprData<'a> {
	Access(Sym),
	// Sym is u32, so this will fit inside a word on a 64-bit system.
	StaticAccess { class_name: Sym, static_method_name: Sym },
	OperatorCall(&'a OperatorCallData<'a>),
	TypeArguments(&'a TypeArgumentsData<'a>),
	//TODO:PERF use separate Call0, Call1, Call2 tags?
	Call(&'a CallData<'a>),
	Recur(List<'a, Expr<'a>>),
	New(&'a NewData<'a>),
	ArrayLiteral(&'a ArrayLiteralData<'a>),
	GetProperty(&'a GetPropertyData<'a>),
	SetProperty(&'a SetPropertyData<'a>),
	Let(&'a LetData<'a>),
	Seq(&'a SeqData<'a>),
	LiteralNat(u32),
	LiteralInt(i32),
	LiteralFloat(f64),
	LiteralString(&'a [u8]),
	SelfExpr,
	IfElse(&'a IfElseData<'a>),
	WhenTest(&'a WhenTestData<'a>),
	Assert(&'a Expr<'a>),
	Try(&'a TryData<'a>),
	For(&'a ForData<'a>),
}

impl<'a> NoDrop for ExprData<'a> {}

#[derive(Serialize)]
pub struct OperatorCallData<'a> {
	pub left: Expr<'a>,
	pub operator: Sym,
	pub right: Expr<'a>,
}
impl<'a> NoDrop for OperatorCallData<'a> {}

#[derive(Serialize)]
pub struct TypeArgumentsData<'a> {
	pub target: Expr<'a>,
	pub type_arguments: List<'a, Ty<'a>>,
}
impl<'a> NoDrop for TypeArgumentsData<'a> {}

#[derive(Serialize)]
pub struct CallData<'a> {
	pub target: Expr<'a>,
	pub args: List<'a, Expr<'a>>,
}
impl<'a> NoDrop for CallData<'a> {}

#[derive(Serialize)]
pub struct NewData<'a>(pub List<'a, Ty<'a>>, pub List<'a, Expr<'a>>);
impl<'a> NoDrop for NewData<'a> {}

#[derive(Serialize)]
pub struct ArrayLiteralData<'a>(pub Option<Ty<'a>>, pub List<'a, Expr<'a>>);
impl<'a> NoDrop for ArrayLiteralData<'a> {}

#[derive(Serialize)]
pub struct GetPropertyData<'a>(pub Expr<'a>, pub Sym);
impl<'a> NoDrop for GetPropertyData<'a> {}

#[derive(Serialize)]
pub struct SetPropertyData<'a>(pub Sym, pub Expr<'a>);
impl<'a> NoDrop for SetPropertyData<'a> {}

#[derive(Serialize)]
pub struct LetData<'a>(pub Pattern<'a>, pub Expr<'a>, pub Late<Expr<'a>>);
impl<'a> NoDrop for LetData<'a> {}

#[derive(Serialize)]
pub struct SeqData<'a>(pub Expr<'a>, pub Expr<'a>);
impl<'a> NoDrop for SeqData<'a> {}

#[derive(Serialize)]
pub struct IfElseData<'a>(pub Expr<'a>, pub Expr<'a>, pub Expr<'a>);
impl<'a> NoDrop for IfElseData<'a> {}

#[derive(Serialize)]
pub struct WhenTestData<'a> {
	pub cases: List<'a, Case<'a>>,
	pub elze: Expr<'a>,
}
impl<'a> NoDrop for WhenTestData<'a> {}

#[derive(Serialize)]
pub struct TryData<'a> {
	pub try: Expr<'a>,
	pub catch: Option<Catch<'a>>,
	pub finally: Option<Expr<'a>>,
}
impl<'a> NoDrop for TryData<'a> {}

#[derive(Serialize)]
pub struct ForData<'a> {
	pub local_name: Sym,
	pub looper: Expr<'a>,
	pub body: Expr<'a>,
}
impl<'a> NoDrop for ForData<'a> {}

#[derive(Serialize)]
pub struct Case<'a>(pub Loc, /*test*/ pub Expr<'a>, /*result*/ pub Expr<'a>);
impl<'a> NoDrop for Case<'a> {}

#[derive(Serialize)]
pub struct Catch<'a> {
	pub loc: Loc,
	pub exception_ty: Ty<'a>,
	pub exception_name_loc: Loc,
	pub exception_name: Sym,
	pub result: Expr<'a>,
}
impl<'a> NoDrop for Catch<'a> {}
