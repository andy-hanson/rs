use serde::{Serialize, Serializer};

use util::arith::to_u8;
use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{LateOwn, Own, Ptr, SerializeAsPtr};
use util::sym::Sym;

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::expr::Expr;
use super::ty::{Ty, TypeParameter};

#[derive(Serialize)]
pub struct MethodSignature {
	pub class: Ptr<ClassDeclaration>,
	pub loc: Loc,
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	pub return_ty: Ty,
	pub self_effect: Effect, // Ignore for static methods
	pub parameters: Arr<Own<Parameter>>,
}
impl MethodSignature {
	pub fn arity(&self) -> u8 {
		to_u8(self.parameters.len())
	}
}

#[derive(Serialize)]
pub struct AbstractMethod(MethodSignature);
impl AbstractMethod {
	pub fn loc(&self) -> Loc {
		self.0.loc
	}

	pub fn name(&self) -> Sym {
		self.0.name
	}

	pub fn type_parameters(&self) -> &[Own<TypeParameter>] {
		&self.0.type_parameters
	}

	pub fn return_ty(&self) -> &Ty {
		&self.0.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.0.self_effect
	}

	pub fn parameters(&self) -> &[Own<Parameter>] {
		&self.0.parameters
	}
}
impl SerializeAsPtr for AbstractMethod {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where S : Serializer {
		self.name().serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct MethodWithBody {
	pub is_static: bool,
	pub signature: MethodSignature,
	// Optional because this might be a builtin method
	pub body: LateOwn<Option<Expr>>,
}
impl MethodWithBody {
	pub fn loc(&self) -> Loc {
		self.signature.loc
	}

	pub fn name(&self) -> Sym {
		self.signature.name
	}

	pub fn type_parameters(&self) -> &[Own<TypeParameter>] {
		&self.signature.type_parameters
	}

	pub fn return_ty(&self) -> &Ty {
		&self.signature.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.signature.self_effect
	}

	pub fn parameters(&self) -> &[Own<Parameter>] {
		&self.signature.parameters
	}

	pub fn body(&self) -> &Option<Expr> {
		&self.body
	}

	pub fn arity(&self) -> u8 {
		self.signature.arity()
	}
}
impl SerializeAsPtr for MethodWithBody {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where S : Serializer {
		self.name().serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Parameter {
	pub loc: Loc,
	pub ty: Ty,
	pub name: Sym,
	pub index: u8,
}
impl SerializeAsPtr for Parameter {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where S : Serializer
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct InstMethod(pub MethodOrAbstract, pub Arr<Ty>);

#[derive(Serialize)]
pub struct Impl {
	pub loc: Loc,
	pub implemented: Ptr<AbstractMethod>,
	pub body: LateOwn<Option<Expr>>,
}

pub enum MethodOrImpl {
	Method(Ptr<MethodWithBody>),
	Impl(Ptr<Impl>),
}
impl MethodOrImpl {
	pub fn copy(&self) -> Self {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrImpl::Method(m.clone_ptr()),
			MethodOrImpl::Impl(ref i) => MethodOrImpl::Impl(i.clone_ptr()),
		}
	}

	pub fn method_or_abstract(&self) -> MethodOrAbstract {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrAbstract::Method(m.clone_ptr()),
			MethodOrImpl::Impl(ref i) => MethodOrAbstract::Abstract(i.implemented.clone_ptr()),
		}
	}

	pub fn return_ty(&self) -> &Ty {
		&self.signature().return_ty
	}

	pub fn signature(&self) -> &MethodSignature {
		match *self {
			MethodOrImpl::Method(ref method) => &method.signature,
			MethodOrImpl::Impl(ref imp) => &imp.implemented.0,
		}
	}

	pub fn arity(&self) -> u8 {
		self.signature().arity()
	}
}
impl Serialize for MethodOrImpl {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where S : Serializer {
		match *self {
			MethodOrImpl::Method(ref m) => m.serialize(serializer),
			MethodOrImpl::Impl(ref i) => i.serialize(serializer),
		}
	}
}


pub enum MethodOrAbstract {
	Method(Ptr<MethodWithBody>),
	Abstract(Ptr<AbstractMethod>),
}
impl MethodOrAbstract {
	pub fn copy(&self) -> Self {
		match *self {
			MethodOrAbstract::Method(ref m) => MethodOrAbstract::Method(m.clone_ptr()),
			MethodOrAbstract::Abstract(ref a) => MethodOrAbstract::Abstract(a.clone_ptr()),
		}
	}

	pub fn is_static(&self) -> bool {
		match *self {
			MethodOrAbstract::Method(ref m) => m.is_static,
			MethodOrAbstract::Abstract(_) => false,
		}
	}

	pub fn type_parameters(&self) -> &[Own<TypeParameter>] {
		match *self {
			MethodOrAbstract::Method(ref m) => m.type_parameters(),
			MethodOrAbstract::Abstract(ref a) => a.type_parameters(),
		}
	}

	pub fn return_ty(&self) -> &Ty {
		match *self {
			MethodOrAbstract::Method(ref m) => m.return_ty(),
			MethodOrAbstract::Abstract(ref a) => a.return_ty(),
		}
	}

	pub fn self_effect(&self) -> Effect {
		match *self {
			MethodOrAbstract::Method(ref m) => m.self_effect(),
			MethodOrAbstract::Abstract(ref a) => a.self_effect(),
		}
	}

	pub fn parameters(&self) -> &[Own<Parameter>] {
		match *self {
			MethodOrAbstract::Method(ref m) => m.parameters(),
			MethodOrAbstract::Abstract(ref a) => a.parameters(),
		}
	}
}
impl Serialize for MethodOrAbstract {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
		where S : Serializer {
		match *self {
			MethodOrAbstract::Method(ref m) => m.serialize(serializer),
			MethodOrAbstract::Abstract(ref a) => a.serialize(serializer),
		}
	}
}
