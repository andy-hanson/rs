use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::arith::usize_to_u8;
use util::late::Late;
use util::loc::Loc;
use util::show::{serialize_as_show, Show, Shower};
use util::sym::Sym;
use util::up::{SerializeUp, Up};

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

#[derive(Serialize)]
pub struct AbstractMethod<'a> {
	pub containing_class: Up<'a, ClassDeclaration<'a>>,
	pub signature: MethodSignature<'a>,
}
impl<'a> AbstractMethod<'a> {
	pub fn loc(&self) -> Loc {
		self.signature.loc
	}

	pub fn name(&self) -> Sym {
		self.signature.name
	}

	pub fn type_parameters(&self) -> &'a [TypeParameter<'a>] {
		self.signature.type_parameters
	}

	pub fn return_ty(&self) -> &Ty<'a> {
		&self.signature.return_ty
	}

	pub fn self_effect(&self) -> Effect {
		self.signature.self_effect
	}

	pub fn parameters(&self) -> &'a [Parameter<'a>] {
		self.signature.parameters
	}

	pub fn arity(&self) -> u8 {
		// + 1 for the 'self' parameter
		usize_to_u8(self.parameters().len()) + 1
	}
}
impl<'a> NoDrop for AbstractMethod<'a> {}
impl<'a> SerializeUp for AbstractMethod<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name().serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct MethodWithBody<'a> {
	pub containing_class: Up<'a, ClassDeclaration<'a>>,
	pub is_static: bool,
	pub signature: MethodSignature<'a>,
	// Optional because this might be a builtin method
	pub body: Late<Option<Expr<'a>>>,
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

	pub fn arity(&self) -> u8 {
		usize_to_u8(self.parameters().len()) + if self.is_static { 0 } else { 1 }
	}
}
impl<'a> SerializeUp for MethodWithBody<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
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
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}

pub struct InstMethod<'a> {
	pub method_decl: MethodOrImplOrAbstract<'a>,
	// Note: this fills in method_decl.containing_class.type_parameters, *and* method_decl.type_parameters.
	pub ty_args: &'a [Late<Ty<'a>>],
	_private: (),
}
impl<'a> InstMethod<'a> {
	pub fn new(method_decl: MethodOrImplOrAbstract<'a>, ty_args: &'a [Late<Ty<'a>>]) -> Self {
		assert!(method_decl.total_type_parameters() == ty_args.len());
		InstMethod { method_decl, ty_args, _private: () }
	}
}
impl<'a> NoDrop for InstMethod<'a> {}
impl<'i, 'a> Show for &'i InstMethod<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.method_decl.name())?;
		if !self.ty_args.is_empty() {
			s.add('[')?;
			s.join(self.ty_args)?;
			s.add(']')?;
		}
		Ok(())
	}
}
impl<'a> Serialize for InstMethod<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serialize_as_show(self, serializer)
	}
}

#[derive(Serialize)]
pub struct Impl<'a> {
	pub loc: Loc,
	pub containing_class: Up<'a, ClassDeclaration<'a>>,
	pub implemented: Up<'a, AbstractMethod<'a>>,
	pub body: Late<Option<Expr<'a>>>,
}
impl<'a> NoDrop for Impl<'a> {}
impl<'a> Impl<'a> {
	pub fn name(&self) -> Sym {
		self.implemented.name()
	}

	pub fn arity(&self) -> u8 {
		self.implemented.arity()
	}
}

#[derive(Copy, Clone)]
pub enum MethodOrImpl<'a> {
	Method(Up<'a, MethodWithBody<'a>>),
	Impl(Up<'a, Impl<'a>>),
}
impl<'a> NoDrop for MethodOrImpl<'a> {}
impl<'a> MethodOrImpl<'a> {
	pub fn containing_class(&self) -> Up<'a, ClassDeclaration<'a>> {
		match *self {
			MethodOrImpl::Method(ref m) => m.containing_class,
			MethodOrImpl::Impl(ref i) => i.containing_class,
		}
	}

	//TODO:just derive Copy
	pub fn copy(&self) -> Self {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrImpl::Method(m.clone_as_up()),
			MethodOrImpl::Impl(ref i) => MethodOrImpl::Impl(i.clone_as_up()),
		}
	}

	pub fn name(&self) -> Sym {
		match *self {
			MethodOrImpl::Method(ref m) => m.name(),
			MethodOrImpl::Impl(ref i) => i.implemented.name(),
		}
	}

	pub fn method_or_abstract(&self) -> MethodOrImplOrAbstract<'a> {
		match *self {
			MethodOrImpl::Method(ref m) => MethodOrImplOrAbstract::Method(m.clone_as_up()),
			MethodOrImpl::Impl(ref i) => MethodOrImplOrAbstract::Abstract(i.implemented.clone_as_up()),
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
			MethodOrImpl::Impl(ref i) => &i.implemented.up_ref().signature,
		}
	}

	pub fn arity(&self) -> u8 {
		match *self {
			MethodOrImpl::Method(m) => m.arity(),
			MethodOrImpl::Impl(i) => i.arity(),
		}
	}
}
impl<'a> Serialize for MethodOrImpl<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		match *self {
			MethodOrImpl::Method(ref m) => m.serialize(serializer),
			MethodOrImpl::Impl(ref i) => i.serialize(serializer),
		}
	}
}
impl<'a> Show for MethodOrImpl<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.containing_class().name)?
			.add('.')?
			.add(self.name())?;
		Ok(())
	}
}

#[derive(Copy, Clone)]
pub enum MethodOrImplOrAbstract<'a> {
	Method(Up<'a, MethodWithBody<'a>>),
	Impl(Up<'a, Impl<'a>>),
	Abstract(Up<'a, AbstractMethod<'a>>),
}
impl<'a> NoDrop for MethodOrImplOrAbstract<'a> {}
impl<'a> MethodOrImplOrAbstract<'a> {
	pub fn containing_class(self) -> Up<'a, ClassDeclaration<'a>> {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.containing_class,
			MethodOrImplOrAbstract::Impl(i) => i.containing_class,
			MethodOrImplOrAbstract::Abstract(a) => a.containing_class,
		}
	}

	pub fn total_type_parameters(self) -> usize {
		match self {
			MethodOrImplOrAbstract::Method(m) => {
				let own = m.type_parameters().len();
				if m.is_static {
					m.containing_class.type_parameters.len() + own
				} else {
					own
				}
			}
			MethodOrImplOrAbstract::Impl(_) => {
				// The Impl itself comes with type parameters -- we just store the instantiations
				unimplemented!()
			}
			MethodOrImplOrAbstract::Abstract(a) =>
				a.containing_class.type_parameters.len() + a.type_parameters().len(),
		}
	}

	/** Includes 1 for "self". */
	pub fn full_arity(self) -> u8 {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.arity(),
			MethodOrImplOrAbstract::Impl(i) => i.implemented.arity(),
			MethodOrImplOrAbstract::Abstract(a) => a.arity(),
		}
	}

	pub fn name(self) -> Sym {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.name(),
			MethodOrImplOrAbstract::Impl(i) => i.implemented.name(),
			MethodOrImplOrAbstract::Abstract(a) => a.name(),
		}
	}

	pub fn is_static(self) -> bool {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.is_static,
			MethodOrImplOrAbstract::Impl(_) | MethodOrImplOrAbstract::Abstract(_) => false,
		}
	}

	pub fn type_parameters(self) -> &'a [TypeParameter<'a>] {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.0.type_parameters(),
			MethodOrImplOrAbstract::Impl(i) => i.implemented.type_parameters(),
			MethodOrImplOrAbstract::Abstract(a) => a.type_parameters(),
		}
	}

	pub fn return_ty(self) -> &'a Ty<'a> {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.0.return_ty(),
			MethodOrImplOrAbstract::Impl(i) => i.0.implemented.return_ty(),
			MethodOrImplOrAbstract::Abstract(a) => a.0.return_ty(),
		}
	}

	pub fn self_effect(self) -> Effect {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.self_effect(),
			MethodOrImplOrAbstract::Impl(i) => i.implemented.self_effect(),
			MethodOrImplOrAbstract::Abstract(a) => a.self_effect(),
		}
	}

	pub fn parameters(self) -> &'a [Parameter<'a>] {
		match self {
			MethodOrImplOrAbstract::Method(m) => m.0.parameters(),
			MethodOrImplOrAbstract::Impl(i) => i.implemented.parameters(),
			MethodOrImplOrAbstract::Abstract(a) => a.parameters(),
		}
	}
}
impl<'a> Serialize for MethodOrImplOrAbstract<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		match *self {
			MethodOrImplOrAbstract::Method(m) => m.serialize(serializer),
			MethodOrImplOrAbstract::Impl(i) => i.serialize(serializer),
			MethodOrImplOrAbstract::Abstract(a) => a.serialize(serializer),
		}
	}
}
