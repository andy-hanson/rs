use serde::{Serialize, Serializer};

use util::arena::{Arena, NoDrop};
use util::iter::KnownLen;
use util::late::Late;
use util::show::{Show, Shower, serialize_as_show};
use util::sym::Sym;
use util::sync::UnsafeSync;
use util::up::{SerializeUp, Up};

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::method::MethodWithBody;

// Make a static version of Bogus so it can be used as a ref
static BOGUS: UnsafeSync<Ty> = UnsafeSync(Ty::Bogus);

pub enum Ty<'a> {
	Bogus,
	Plain(Effect, InstClass<'a>),
	Param(Up<'a, TypeParameter<'a>>),
}
impl<'a> NoDrop for Ty<'a> {}
impl<'a> Ty<'a> {
	// Used for getting a *reference* to Ty::Bogus.
	pub fn bogus_ref() -> &'a Self {
		unused!(BOGUS);
		unimplemented!() //&BOGUS.0
	}

	pub fn pure_ty(class: InstClass<'a>) -> Self {
		Ty::Plain(Effect::Pure, class)
	}

	pub fn io(class: InstClass<'a>) -> Self {
		Ty::Plain(Effect::Io, class)
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		match *self {
			Ty::Bogus =>
				// TODO: this should probably always be true, but then don't call this fn "equals"
				match *other { Ty::Bogus => true, _ => false },
			Ty::Plain(effect, ref inst_class) => {
				if let Ty::Plain(effect_b, ref inst_class_b) = *other {
					effect == effect_b && inst_class.fast_equals(inst_class_b)
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
}
impl<'a> Clone for Ty<'a> {
	fn clone(&self) -> Self {
		match *self {
			Ty::Bogus => Ty::Bogus,
			Ty::Plain(effect, ref inst_class) => Ty::Plain(effect, inst_class.clone()),
			Ty::Param(tp) => Ty::Param(tp),
		}
	}
}
impl<'t, 'a> Show for &'t Ty<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			Ty::Bogus => {
				s.add("<bogus>")?;
			}
			Ty::Plain(effect, ref inst_class) => {
				s.add(effect)?.add(' ')?.add(inst_class)?;
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

#[derive(Clone, Serialize)]
pub struct InstClass<'a> {
	pub class: Up<'a, ClassDeclaration<'a>>,
	pub ty_args: &'a [Ty<'a>],
}
impl<'a> InstClass<'a> {
	pub fn generic_self_reference(class: Up<'a, ClassDeclaration<'a>>, arena: &'a Arena) -> Self {
		let ty_args = arena.map(class.type_parameters, |tp| Ty::Param(Up(tp)));
		InstClass { class, ty_args }
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		self.class.ptr_eq(other.class) && self.ty_args.each_equals(other.ty_args, Ty::fast_equals)
	}
}
impl<'a> NoDrop for InstClass<'a> {}
impl<'i, 'a> Show for &'i InstClass<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.class.name)?;
		if !self.ty_args.is_empty() {
			s.add('[')?;
			s.join(self.ty_args)?;
			s.add(']')?;
		}
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
