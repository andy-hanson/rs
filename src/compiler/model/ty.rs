use util::arr::Arr;
use util::ptr::{ Own, Ptr, LateOwn };
use util::sym::Sym;

use super::class_declaration::ClassDeclaration;
use super::method::MethodWithBody;
use super::effect::Effect;

pub enum Ty {
	Bogus,
	Plain(Effect, InstCls),
	Param(Ptr<TypeParameter>),
}
impl Ty {
	pub fn pure_ty(cls: InstCls) -> Ty {
		Ty::Plain(Effect::Pure, cls)
	}

	pub fn io(cls: InstCls) -> Ty {
		Ty::Plain(Effect::Io, cls)
	}
}
impl Clone for Ty {
	fn clone(&self) -> Ty {
		match self {
			&Ty::Bogus => Ty::Bogus,
			&Ty::Plain(ref effect, ref inst_cls) => Ty::Plain(effect.clone(), inst_cls.clone()),
			&Ty::Param(ref tp) => Ty::Param(tp.clone_ptr()),
		}
	}
}

pub struct InstCls {
	pub class: Ptr<ClassDeclaration>,
	pub type_arguments: Arr<Ty>,
}
impl Clone for InstCls {
	fn clone(&self) -> InstCls {
		InstCls { class: self.class.clone_ptr(), type_arguments: self.type_arguments.clone() }
	}
}

pub enum TypeParameterOrigin {
	Class(Ptr<ClassDeclaration>),
	Method(Ptr<MethodWithBody>)
}
impl<'a> TypeParameterOrigin {
	fn clone(&self) -> TypeParameterOrigin {
		match self {
			&TypeParameterOrigin::Class(ref cls) => {
				let cln = cls.clone();
				TypeParameterOrigin::Class(cln.clone_ptr())
			}
			&TypeParameterOrigin::Method(ref m) => {
				TypeParameterOrigin::Method(m.clone_ptr())
			}
		}
	}
}

pub struct TypeParameter {
	origin: LateOwn<TypeParameterOrigin>,
	name: Sym,
}
impl TypeParameter {
	pub fn create(name: Sym) -> Own<TypeParameter> {
		Own::new(TypeParameter { origin: LateOwn::new(), name })
	}

	pub fn set_origins(type_parameters: &Arr<Own<TypeParameter>>, origin: TypeParameterOrigin) {
		for tp in type_parameters.iter() {
			tp.origin.init(origin.clone())
		}
	}

	pub fn fast_equals(&self, other: &TypeParameter) -> bool {
		// TODO: self and other should be `Ref`, then use reference identity.
		// See https://www.reddit.com/r/rust/comments/2dmzf6/why_do_equality_tests_of_references_seem_to/
		unused!(other);
		todo!()
	}
}
