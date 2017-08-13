use model::{ Effect, LiteralValue };
use util::arr::Arr;
use util::loc::Loc;
use util::path::{ Path, RelPath };
use util::sym::Sym;

struct Module {
	imports: Arr<Import>,
	class: Box<ClassDeclaration>,
}

enum Import {
	Global(Loc, Path),
	Local(Loc, RelPath),
}

struct ClassDeclaration {
	type_parameters: Arr<Sym>,
	head: Option<Head>,
	supers: Arr<Super>,
	methods: Arr<Method>,
}

enum Head {
	Abstract(Arr<AbstractMethod>),
	Slots(Arr<Slot>),
}

struct AbstractMethod {
	loc: Loc,
	return_ty: Box<Ty>,
	name: Sym,
	type_parameters: Arr<Sym>,
	self_effect: Effect,
	parameters: Arr<Parameter>,
}

struct Slot {
	loc: Loc,
	mutable: bool,
	ty: Box<Ty>,
	name: Sym,
}

struct Super {
	loc: Loc,
	name: Sym,
	ty_args: Arr<Ty>,
	impls: Arr<Impl>,
}

struct Impl {
	loc: Loc,
	name: Sym,
	parameter_names: Arr<Sym>,
	body: Expr,
}

struct Method {
	loc: Loc,
	is_static: bool,
	type_parameters: Arr<Sym>,
	return_ty: Ty,
	name: Sym,
	self_effect: Effect,
	parameters: Arr<Parameter>,
	body: Expr,
}

struct Parameter {
	loc: Loc,
	ty: Box<Ty>,
	name: Sym,
}

pub struct Ty {
	pub loc: Loc,
	pub effect: Effect,
	pub name: Sym,
	pub ty_args: Arr<Ty>,
}

struct Pattern {
	loc: Loc,
	data: PatternData,
}
enum PatternData {
	Ignore,
	Single(Sym),
	Destruct(Arr<Pattern>),
}

struct Expr {
	loc: Loc,
	data: ExprData,
}
enum ExprData {
	Access(Sym),
	StaticAccess(/*class_name*/ Sym, /*static_method_name*/Sym),
	OperatorCall(Box<Expr>, Sym, Box<Expr>),
	TypeArguments(Box<Expr>, Arr<Ty>),
	Call(Box<Expr>, Arr<Expr>),
	Recur(Arr<Expr>),
	New(Arr<Ty>, Arr<Expr>),
	ArrayLiteral(Option<Ty>, Arr<Expr>),
	GetProperty(Box<Expr>, Sym),
	SetProperty(Sym, Box<Expr>),
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

struct Case {
	loc: Loc,
	test: Expr,
	result: Expr,
}

struct Catch {
	loc: Loc,
	exception_ty: Ty,
	exception_name_loc: Loc,
	exception_name: Sym,
	then: Box<Expr>,
}
