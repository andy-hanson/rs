use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::late::Late;
use util::show::{serialize_as_show, Show, Shower};
use util::sym::Sym;
use util::up::{SerializeUp, Up};

use super::class::{ClassDeclaration, InstClass};
use super::effect::Effect;
use super::method::MethodWithBody;

#[derive(Hash)]
pub enum Ty<'a> {
	Bogus,
	Plain(PlainTy<'a>),
	Param(Up<'a, TypeParameter<'a>>),
}
impl<'a> NoDrop for Ty<'a> {}
impl<'a> Ty<'a> {
	pub fn pure_ty(inst_class: InstClass<'a>) -> Self {
		Ty::Plain(PlainTy { effect: Effect::Pure, inst_class })
	}

	pub fn io(inst_class: InstClass<'a>) -> Self {
		Ty::Plain(PlainTy { effect: Effect::Io, inst_class })
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		match *self {
			Ty::Bogus =>
				// TODO: this should probably always be true, but then don't call this fn "equals"
				match *other { Ty::Bogus => true, _ => false },
			Ty::Plain(PlainTy { effect: effect_a, inst_class: ref inst_class_a }) => {
				if let Ty::Plain(PlainTy { effect: effect_b, inst_class: ref inst_class_b }) = *other {
					effect_a == effect_b && inst_class_a.fast_equals(inst_class_b)
				} else {
					false
				}
			}
			Ty::Param(p_a) =>
				if let Ty::Param(p_b) = *other {
					p_a.ptr_eq(p_b)
				} else {
					false
				}
		}
	}

	pub fn assert_plain(&self) -> &PlainTy<'a> {
		match *self {
			Ty::Plain(ref p) => p,
			_ => unreachable!(),
		}
	}
}
impl<'a> Clone for Ty<'a> {
	fn clone(&self) -> Self {
		match *self {
			Ty::Bogus => Ty::Bogus,
			Ty::Plain(ref plain) => Ty::Plain(plain.clone()),
			Ty::Param(tp) => Ty::Param(tp),
		}
	}
}
impl<'t, 'a> Show for &'t Ty<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			Ty::Bogus => {
				s.add("<Bogus>")?;
			}
			Ty::Plain(ref p) => {
				s.add(p)?;
			}
			Ty::Param(p) => {
				s.add(p.name)?;
			}
		}
		Ok(())
	}
}
impl<'a> Serialize for Ty<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serialize_as_show(self, serializer)
	}
}

#[derive(Clone, Hash)]
pub struct PlainTy<'a> {
	pub effect: Effect,
	pub inst_class: InstClass<'a>,
}
impl<'t, 'a> Show for &'t PlainTy<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.effect)?.add(' ')?.add(&self.inst_class)?;
		Ok(())
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
			TypeParameterOrigin::Class(ref class) => TypeParameterOrigin::Class(class.clone_as_up()),
			TypeParameterOrigin::Method(ref m) => TypeParameterOrigin::Method(m.clone_as_up()),
		}
	}
}

pub struct TypeParameter<'a> {
	pub origin: Late<TypeParameterOrigin<'a>>,
	pub name: Sym,
}
impl<'a> NoDrop for TypeParameter<'a> {}
impl<'a> TypeParameter<'a> {
	pub fn create(name: Sym) -> Self {
		TypeParameter { origin: Late::new(), name }
	}

	pub fn set_origins(type_parameters: &[TypeParameter<'a>], origin: TypeParameterOrigin<'a>) {
		for tp in type_parameters {
			&tp.origin <- origin.copy();
		}
	}
}
impl<'a> Serialize for TypeParameter<'a> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		// We just seralized the origin, so skip that.
		self.name.serialize(serializer)
	}
}
impl<'a> SerializeUp for TypeParameter<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}
