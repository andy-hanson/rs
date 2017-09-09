use serde::{Serialize, Serializer};

use util::arena::{ptr_eq, Arena, NoDrop, SerializeUp, Up};
use util::arr::SliceOps;
use util::late::Late;
use util::sync::UnsafeSync;
use util::sym::Sym;

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::method::MethodWithBody;

// Make a static version of Bogus so it can be used as a ref
static BOGUS: UnsafeSync<Ty> = UnsafeSync(Ty::Bogus);

#[derive(Serialize)]
pub enum Ty<'a> {
	Bogus,
	Plain(Effect, InstCls<'a>),
	Param(&'a TypeParameter<'a>),
}
impl<'a> NoDrop for Ty<'a> {}
impl<'a> Ty<'a> {
	// Used for getting a *reference* to Ty::Bogus.
	pub fn bogus_ref() -> &'a Self {
		unused!(BOGUS);
		unimplemented!() //&BOGUS.0
	}

	pub fn pure_ty(cls: InstCls<'a>) -> Self {
		Ty::Plain(Effect::Pure, cls)
	}

	pub fn io(cls: InstCls<'a>) -> Self {
		Ty::Plain(Effect::Io, cls)
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		match *self {
			Ty::Bogus =>
				// TODO: this should probably always be true, but then don't call this fn "equals"
				match *other { Ty::Bogus => true, _ => false },
			Ty::Plain(effect, ref inst_cls) => {
				if let Ty::Plain(effect_b, ref inst_cls_b) = *other {
					effect == effect_b && inst_cls.fast_equals(inst_cls_b)
				} else {
					false
				}
			}
			Ty::Param(p) =>
				if let Ty::Param(p_b) = *other {
					ptr_eq(p, p_b)
				} else {
					false
				}
		}
	}
}
impl<'a> Clone for Ty<'a> {
	fn clone(&self) -> Self {
		match *self {
			Ty::Bogus => Ty::Bogus,
			Ty::Plain(effect, ref inst_cls) => Ty::Plain(effect, inst_cls.clone()),
			Ty::Param(tp) => Ty::Param(tp),
		}
	}
}
/*impl Serialize for Ty {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer
	{
		match *self {
			Ty::Bogus => serializer.serialize_unit_variant("Ty", 0, "Bogus"),
			Ty::Plain(_, _) => {
				unimplemented!()
			}
			Ty::Param(_) => {
				//Don't serialize the param normally, we're just referencing it!
				unimplemented!()
			}
		}
	}
}*/

#[derive(Serialize)]
pub struct InstCls<'a>(pub &'a ClassDeclaration<'a>, pub &'a [Ty<'a>]);
impl<'a> InstCls<'a> {
	pub fn generic_self_reference(cls: &'a ClassDeclaration<'a>, arena: &'a Arena) -> Self {
		let type_arguments = cls.type_parameters.map(arena, |tp| Ty::Param(tp));
		InstCls(cls, type_arguments)
	}

	pub fn class(&self) -> &'a ClassDeclaration<'a> {
		self.0
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		ptr_eq(self.0, other.0) && self.1.each_equals(other.1, Ty::fast_equals)
	}
}
impl<'a> NoDrop for InstCls<'a> {}
impl<'a> Clone for InstCls<'a> {
	fn clone(&self) -> Self {
		InstCls(self.0, self.1)
	}
}

pub enum TypeParameterOrigin<'a> {
	Class(Up<'a, ClassDeclaration<'a>>),
	Method(Up<'a, MethodWithBody<'a>>),
}
impl<'a> NoDrop for TypeParameterOrigin<'a> {}
impl<'a> TypeParameterOrigin<'a> {
	//TODO:just derive Copy
	fn copy(&self) -> Self {
		match *self {
			TypeParameterOrigin::Class(ref cls) => TypeParameterOrigin::Class(cls.clone_as_up()),
			TypeParameterOrigin::Method(ref m) => TypeParameterOrigin::Method(m.clone_as_up()),
		}
	}
}

pub struct TypeParameter<'a> {
	origin: Late<TypeParameterOrigin<'a>>,
	name: Sym,
}
impl<'a> NoDrop for TypeParameter<'a> {}
impl<'a> TypeParameter<'a> {
	pub fn create(name: Sym) -> Self {
		TypeParameter { origin: Late::new(), name }
	}

	pub fn set_origins(type_parameters: &[TypeParameter<'a>], origin: TypeParameterOrigin<'a>) {
		for tp in type_parameters.iter() {
			tp.origin.init(origin.copy());
		}
	}

	pub fn fast_equals(&self, other: &TypeParameter) -> bool {
		// TODO: self and other should be `Ref`, then use reference identity.
		// See https://www.reddit.com/r/rust/comments/2dmzf6/why_do_equality_tests_of_references_seem_to/
		unused!(other);
		unimplemented!()
	}
}
impl<'a> Serialize for TypeParameter<'a> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		// We just seralized the origin, so skip that.
		self.name.serialize(serializer)
	}
}
impl<'a> SerializeUp for TypeParameter<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}
