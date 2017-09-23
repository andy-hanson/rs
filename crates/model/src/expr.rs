use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::late::Late;
use util::loc::Loc;
use util::sym::Sym;
use util::up::{SerializeUp, Up};

use super::class::SlotDeclaration;
use super::method::{InstMethod, MethodOrImpl, Parameter};
use super::ty::Ty;

#[derive(Serialize)]
pub enum Pattern<'a> {
	Ignore,
	Single(Local<'a>),
	Destruct(Loc, &'a [Pattern<'a>]),
}
impl<'a> NoDrop for Pattern<'a> {}

#[derive(Serialize)]
pub struct Local<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for Local<'a> {}
impl<'a> SerializeUp for Local<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Case<'a> {
	pub loc: Loc,
	pub test: Expr<'a>,
	pub result: Expr<'a>,
}
impl<'a> NoDrop for Case<'a> {}

#[derive(Serialize)]
pub struct Catch<'a> {
	pub loc: Loc,
	pub caught: Local<'a>,
	pub result: Late<Expr<'a>>,
}
impl<'a> NoDrop for Catch<'a> {}

#[derive(Serialize)]
pub struct Expr<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub data: ExprData<'a>,
}
impl<'a> Expr<'a> {
	//pub fn children(&self) -> &[Self] {
 //	self.1.children()
 //}
}
impl<'a> NoDrop for Expr<'a> {}

#[derive(Serialize)]
// All variants should be 1 word in size (excluding the tag).
pub enum ExprData<'a> {
	Bogus,
	AccessParameter(Up<'a, Parameter<'a>>),
	AccessLocal(Up<'a, Local<'a>>),
	Let(&'a LetData<'a>),
	Seq(&'a SeqData<'a>),
	LiteralNat(u32),
	LiteralInt(i32),
	LiteralFloat(f64),
	LiteralString(&'a [u8]),
	IfElse(&'a IfElseData<'a>),
	WhenTest(&'a WhenTestData<'a>),
	Try(&'a TryData<'a>),
	For(&'a ForData<'a>),
	StaticMethodCall(&'a StaticMethodCallData<'a>),
	InstanceMethodCall(&'a InstanceMethodCallData<'a>),
	MyInstanceMethodCall(&'a MyInstanceMethodCallData<'a>),
	New(&'a [Expr<'a>]),
	ArrayLiteral(&'a [&'a Expr<'a>]),
	GetMySlot(Up<'a, SlotDeclaration<'a>>),
	GetSlot(&'a GetSlotData<'a>),
	SetSlot(&'a SetSlotData<'a>),
	SelfExpr,
	Assert(&'a Expr<'a>),
	Recur(&'a RecurData<'a>),
}
impl<'a> NoDrop for ExprData<'a> {}
impl<'a> ExprData<'a> {
	/*pub fn children(&self) -> &'a [&'a Expr<'a>] {
		match *self {
			ExprData::Let(_, ref a, ref b)
				| ExprData::Seq(ref a, ref b)
				=> Arr::_2(a, b),
			ExprData::IfElse { ref test, ref then, ref elze, .. } =>
				Arr::_3(test, then, elze),
			ExprData::WhenTest(ref cases, ref elze, _) => {
				unimplemented!()
				/*let mut b = ArrBuilder::<&Expr>::new();
				for &Case(_, ref test, ref result) in cases.iter() {
					b.add(test);
					b.add(result)
				}
				b.add(elze);
				b.finish()
				*/
			}
			ExprData::Try { ref body, ref catch, ref finally, .. } =>
				match *catch {
					Some(Catch(_, _, ref catch_result)) =>
						match *finally {
							Some(ref f) => Arr::_3(body, catch_result, f),
							None => Arr::_2(body, catch_result)
						},
					None =>
						match *finally { Some(ref f) => Arr::_2(body, f), None => Arr::_1(body) }
				},
			ExprData::For { ref looper, ref body, .. } => Arr::_2(looper, body),
			ExprData::InstanceMethodCall(ref target, _, ref args, _) => {
				//let mut b = ArrBuilder::<&Expr>::new();
				//b.add(target);
				//add_refs(&mut b, args);
				//b.finish()
				unimplemented!()
			}
			ExprData::GetSlot(ref e, _, _)
				| ExprData::SetSlot(_, ref e)
				| ExprData::Assert(ref e)
				=> Arr::_1(e),
			ExprData::ArrayLiteral { elements: ref args, .. }
				| ExprData::New(_, ref args)
				| ExprData::MyInstanceMethodCall(_, ref args, _)
				| ExprData::StaticMethodCall(_, ref args, _)
				//| &ExprData::RecurMethod(_, ref args)
				//| &ExprData::RecurImpl(_, ref args)
				| ExprData::Recur(_, ref args)
				=> unimplemented!(),//make_refs(args),
			ExprData::Bogus
				| ExprData::AccessParameter(_, _)
				| ExprData::AccessLocal(_)
				| ExprData::Literal(_)
				| ExprData::GetMySlot(_, _)
				| ExprData::SelfExpr(_)
				=> Arr::empty(),
		}
	}*/
}

#[derive(Serialize)]
pub struct SeqData<'a>(pub Expr<'a>, pub Expr<'a>);
impl<'a> NoDrop for SeqData<'a> {}

#[derive(Serialize)]
pub struct IfElseData<'a> {
	pub test: Expr<'a>,
	pub then: Expr<'a>,
	pub elze: Expr<'a>,
}
impl<'a> NoDrop for IfElseData<'a> {}

#[derive(Serialize)]
pub struct WhenTestData<'a> {
	pub cases: &'a [Case<'a>],
	pub elze: Expr<'a>,
}
impl<'a> NoDrop for WhenTestData<'a> {}

#[derive(Serialize)]
pub struct StaticMethodCallData<'a> {
	pub method: InstMethod<'a>,
	pub args: &'a [Expr<'a>],
}
impl<'a> NoDrop for StaticMethodCallData<'a> {}

#[derive(Serialize)]
pub struct LetData<'a> {
	pub pattern: Late<Pattern<'a>>,
	pub value: Expr<'a>,
	pub then: Late<Expr<'a>>,
}
impl<'a> NoDrop for LetData<'a> {}

#[derive(Serialize)]
pub struct InstanceMethodCallData<'a> {
	pub target: Expr<'a>,
	pub method: InstMethod<'a>,
	pub args: &'a [Expr<'a>],
}
impl<'a> NoDrop for InstanceMethodCallData<'a> {}

#[derive(Serialize)]
pub struct MyInstanceMethodCallData<'a> {
	pub method: InstMethod<'a>,
	pub args: &'a [Expr<'a>],
}
impl<'a> NoDrop for MyInstanceMethodCallData<'a> {}

#[derive(Serialize)]
pub struct GetSlotData<'a> {
	pub target: Expr<'a>,
	pub slot: Up<'a, SlotDeclaration<'a>>,
}
impl<'a> NoDrop for GetSlotData<'a> {}

#[derive(Serialize)]
pub struct SetSlotData<'a>(pub Up<'a, SlotDeclaration<'a>>, pub Expr<'a>);
impl<'a> NoDrop for SetSlotData<'a> {}

#[derive(Serialize)]
pub struct RecurData<'a>(pub MethodOrImpl<'a>, pub &'a [Expr<'a>]);
impl<'a> NoDrop for RecurData<'a> {}

#[derive(Serialize)]
pub struct TryData<'a> {
	pub body: Expr<'a>,
	pub catch: Option<Catch<'a>>,
	pub finally: Option<Expr<'a>>,
}
impl<'a> NoDrop for TryData<'a> {}

#[derive(Serialize)]
pub struct ForData<'a> {
	pub local: Local<'a>,
	pub looper: Expr<'a>,
	pub body: Expr<'a>,
	pub provided_ty: Ty<'a>,
	pub received_ty: Ty<'a>,
}
impl<'a> NoDrop for ForData<'a> {}
