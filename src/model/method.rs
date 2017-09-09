use serde::{Serialize, Serializer};

use util::arena::{NoDrop, SerializeUp, Up};
use util::arith::to_u8;
use util::late::Late;
use util::loc::Loc;
use util::sym::Sym;

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::expr::Expr;
use super::ty::{Ty, TypeParameter};

#[derive(Serialize)]
pub struct MethodSignature<'a> {
	pub class: Up<'a, ClassDeclaration<'a>>,
	pub loc: Loc,
	pub name: Sym,
	pub type_parameters: &'a [TypeParameter<'a>],
	pub return_ty: Ty<'a>,
	pub self_effect: Effect, // Ignore for static methods
	pub parameters: &'a [Parameter<'a>],
}
impl<'a> NoDrop for MethodSignature<'a> {}
impl<'a> MethodSignature<'a> {
	pub fn arity(&self) -> u8 {
		to_u8(self.parameters.len())
	}
}

#[derive(Serialize)]
pub struct AbstractMethod<'a>(MethodSignature<'a>);
impl<'a> AbstractMethod<'a> {
	pub fn loc(&self) -> Loc {
		self.0.loc
	}

	pub fn name(&self) -> Sym {
		self.0.name
	}

	pub fn type_parameters(&self) -> &'a [TypeParameter<'a>] {
		self.0.type_parameters
	}

	pub fn return_ty(&self) -> &Ty<'a> {
		&self.0.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.0.self_effect
	}

	pub fn parameters(&self) -> &'a [Parameter<'a>] {
		self.0.parameters
	}
}
impl<'a> NoDrop for AbstractMethod<'a> {}
impl<'a> SerializeUp for AbstractMethod<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name().serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct MethodWithBody<'a> {
	pub is_static: bool,
	pub signature: MethodSignature<'a>,
	// Optional because this might be a builtin method
	//TODO:PERF should own the expr, not point to it
	pub body: Late<Option<&'a Expr<'a>>>,
}
impl<'a> NoDrop for MethodWithBody<'a> {}
impl<'a> MethodWithBody<'a> {
	pub fn loc(&self) -> Loc {
		self.signature.loc
	}

	pub fn name(&self) -> Sym {
		self.signature.name
	}

	pub fn type_parameters(&self) -> &[TypeParameter<'a>] {
		self.signature.type_parameters
	}

	pub fn return_ty(&self) -> &Ty<'a> {
		&self.signature.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.signature.self_effect
	}

	pub fn parameters(&self) -> &[Parameter<'a>] {
		self.signature.parameters
	}

	pub fn body(&self) -> Option<&'a Expr<'a>> {
		*self.body
	}

	pub fn arity(&self) -> u8 {
		self.signature.arity()
	}
}
impl<'a> SerializeUp for MethodWithBody<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name().serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Parameter<'a> {
	pub loc: Loc,
	pub ty: Ty<'a>,
	pub name: Sym,
	pub index: u8,
}
impl<'a> NoDrop for Parameter<'a> {}
impl<'a> SerializeUp for Parameter<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct InstMethod<'a>(pub MethodOrAbstract<'a>, pub &'a [Ty<'a>]);
impl<'a> NoDrop for InstMethod<'a> {}

#[derive(Serialize)]
pub struct Impl<'a> {
	pub loc: Loc,
	pub implemented: Up<'a, AbstractMethod<'a>>,
	//TODO:PERF should own the body, not reference it
	pub body: Late<Option<&'a Expr<'a>>>,
}
impl<'a> NoDrop for Impl<'a> {}

pub enum MethodOrImpl<'a> {
	Method(Up<'a, MethodWithBody<'a>>),
	Impl(Up<'a, Impl<'a>>),
}
impl<'a> NoDrop for MethodOrImpl<'a> {}
impl<'a> MethodOrImpl<'a> {
	//TODO:just derive Copy
	pub fn copy(&self) -> Self {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrImpl::Method(m.clone_as_up()),
			MethodOrImpl::Impl(ref i) => MethodOrImpl::Impl(i.clone_as_up()),
		}
	}

	pub fn method_or_abstract(&self) -> MethodOrAbstract<'a> {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrAbstract::Method(m.clone_as_up()),
			MethodOrImpl::Impl(ref i) => MethodOrAbstract::Abstract(i.implemented.clone_as_up()),
		}
	}

	pub fn return_ty(&self) -> &Ty<'a> {
		&self.signature().return_ty
	}

	pub fn signature(&self) -> &'a MethodSignature<'a> {
		//TODO:shouldn't need copy
		match *self {
			MethodOrImpl::Method(ref m) => {
				let x: &'a MethodWithBody<'a> = m.0;
				&x.signature
			}
			MethodOrImpl::Impl(ref i) => &(i.implemented.0).0,
		}
	}

	pub fn arity(&self) -> u8 {
		self.signature().arity()
	}
}
impl<'a> Serialize for MethodOrImpl<'a> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		match *self {
			MethodOrImpl::Method(ref m) => m.serialize(serializer),
			MethodOrImpl::Impl(ref i) => i.serialize(serializer),
		}
	}
}


pub enum MethodOrAbstract<'a> {
	Method(Up<'a, MethodWithBody<'a>>),
	Abstract(Up<'a, AbstractMethod<'a>>),
}
impl<'a> NoDrop for MethodOrAbstract<'a> {}
impl<'a> MethodOrAbstract<'a> {
	pub fn copy(&self) -> Self {
		match *self {
			MethodOrAbstract::Method(ref m) => MethodOrAbstract::Method(m.clone_as_up()),
			MethodOrAbstract::Abstract(ref a) => MethodOrAbstract::Abstract(a.clone_as_up()),
		}
	}

	pub fn is_static(&self) -> bool {
		match *self {
			MethodOrAbstract::Method(ref m) => m.is_static,
			MethodOrAbstract::Abstract(_) => false,
		}
	}

	pub fn type_parameters(&self) -> &'a [TypeParameter<'a>] {
		match *self {
			MethodOrAbstract::Method(ref m) => m.0.type_parameters(),
			MethodOrAbstract::Abstract(ref a) => a.0.type_parameters(),
		}
	}

	pub fn return_ty(&self) -> &Ty<'a> {
		match *self {
			MethodOrAbstract::Method(ref m) => m.0.return_ty(),
			MethodOrAbstract::Abstract(ref a) => a.0.return_ty(),
		}
	}

	pub fn self_effect(&self) -> Effect {
		match *self {
			MethodOrAbstract::Method(ref m) => m.0.self_effect(),
			MethodOrAbstract::Abstract(ref a) => a.0.self_effect(),
		}
	}

	pub fn parameters(&self) -> &'a [Parameter<'a>] {
		match *self {
			MethodOrAbstract::Method(ref m) => m.0.parameters(),
			MethodOrAbstract::Abstract(ref a) => a.0.parameters(),
		}
	}
}
impl<'a> Serialize for MethodOrAbstract<'a> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		match *self {
			MethodOrAbstract::Method(ref m) => m.serialize(serializer),
			MethodOrAbstract::Abstract(ref a) => a.serialize(serializer),
		}
	}
}
