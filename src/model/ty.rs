use util::arr::{Arr, SliceOps};
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::class::ClassDeclaration;
use super::effect::Effect;
use super::method::MethodWithBody;

// Make a static version of Bogus so it can be used as a ref
static BOGUS: UnsafeSync<Ty> = UnsafeSync(Ty::Bogus);
struct UnsafeSync<T>(T);
unsafe impl<T> Sync for UnsafeSync<T> {}

pub enum Ty {
	Bogus,
	Plain(Effect, InstCls),
	Param(Ptr<TypeParameter>),
}
impl Ty {
	// Used for getting a *reference* to Ty::Bogus.
	pub fn bogus_ref() -> &'static Self {
		&BOGUS.0
	}

	pub fn pure_ty(cls: InstCls) -> Self {
		Ty::Plain(Effect::Pure, cls)
	}

	pub fn io(cls: InstCls) -> Self {
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
			Ty::Param(ref p) =>
				if let Ty::Param(ref p_b) = *other {
					p.ptr_equals(p_b)
				} else {
					false
				}
		}
	}
}
impl Clone for Ty {
	fn clone(&self) -> Self {
		match *self {
			Ty::Bogus => Ty::Bogus,
			Ty::Plain(effect, ref inst_cls) => Ty::Plain(effect, inst_cls.clone()),
			Ty::Param(ref tp) => Ty::Param(tp.clone_ptr()),
		}
	}
}

pub struct InstCls(pub Ptr<ClassDeclaration>, pub Arr<Ty>);
impl InstCls {
	pub fn generic_self_reference(cls: Ptr<ClassDeclaration>) -> Self {
		let type_arguments = cls.type_parameters.map(|tp| Ty::Param(tp.ptr()));
		InstCls(cls, type_arguments)
	}

	pub fn class(&self) -> &Ptr<ClassDeclaration> {
		&self.0
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		self.0.ptr_equals(&other.0) && self.1.each_equals(&other.1, Ty::fast_equals)
	}
}
impl Clone for InstCls {
	fn clone(&self) -> Self {
		InstCls(self.0.clone_ptr(), self.1.clone())
	}
}

pub enum TypeParameterOrigin {
	Class(Ptr<ClassDeclaration>),
	Method(Ptr<MethodWithBody>),
}
impl<'a> TypeParameterOrigin {
	fn copy(&self) -> Self {
		match *self {
			TypeParameterOrigin::Class(ref cls) => TypeParameterOrigin::Class(cls.clone_ptr()),
			TypeParameterOrigin::Method(ref m) => TypeParameterOrigin::Method(m.clone_ptr()),
		}
	}
}

pub struct TypeParameter {
	origin: LateOwn<TypeParameterOrigin>,
	name: Sym,
}
impl TypeParameter {
	pub fn create(name: Sym) -> Own<Self> {
		Own::new(TypeParameter { origin: LateOwn::new(), name })
	}

	pub fn set_origins(type_parameters: &[Own<TypeParameter>], origin: TypeParameterOrigin) {
		for tp in type_parameters.iter() {
			tp.origin.init(origin.copy())
		}
	}

	pub fn fast_equals(&self, other: &TypeParameter) -> bool {
		// TODO: self and other should be `Ref`, then use reference identity.
		// See https://www.reddit.com/r/rust/comments/2dmzf6/why_do_equality_tests_of_references_seem_to/
		unused!(other);
		unimplemented!()
	}
}