use util::sym::Sym;

use super::super::super::model::class::{ClassDeclaration, ClassHead, MemberDeclaration};
use super::super::super::model::ty::InstCls;

use super::instantiator::Instantiator;

pub fn try_get_member_from_class_declaration<'a>(cls: &ClassDeclaration<'a>, member_name: Sym) -> Option<InstMember<'a>> {
	get_member_worker(cls, Instantiator::NIL, member_name)
}

pub fn try_get_member_of_inst_cls<'a>(cls: &InstCls<'a>, member_name: Sym) -> Option<InstMember<'a>> {
	get_member_worker(cls.class(), Instantiator::of_inst_cls(cls), member_name)
}

fn get_member_worker<'a>(
	cls: &ClassDeclaration<'a>,
	instantiator: Instantiator<'a>,
	member_name: Sym,
) -> Option<InstMember<'a>> {
	for method in cls.methods.iter() {
		if method.name() == member_name {
			return Some(InstMember(MemberDeclaration::Method(&method), instantiator))
		}
	}

	match *cls.head {
		ClassHead::Static | ClassHead::Builtin => {}
		ClassHead::Slots(_, ref slots) =>
			for slot in slots.iter() {
				if slot.name == member_name {
					return Some(InstMember(MemberDeclaration::Slot(slot), instantiator))
				}
			},
		ClassHead::Abstract(_, ref methods) =>
			for method in methods.iter() {
				if method.name() == member_name {
					return Some(InstMember(MemberDeclaration::AbstractMethod(method), instantiator))
				}
			},
	}

	for zuper in cls.supers.iter() {
		let super_instantiator = instantiator.combine(&Instantiator::of_inst_cls(&zuper.super_class));
		let got = get_member_worker(zuper.super_class.class(), super_instantiator, member_name);
		if got.is_some() {
			return got
		}
	}

	None
}

pub struct InstMember<'a>(pub MemberDeclaration<'a>, pub Instantiator<'a>);
