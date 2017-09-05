use util::sym::Sym;

use super::super::super::model::class::{ClassDeclaration, ClassHead, MemberDeclaration};
use super::super::super::model::ty::InstCls;

use super::instantiator::Instantiator;

pub fn try_get_member_from_class_declaration(cls: &ClassDeclaration, member_name: Sym) -> Option<InstMember> {
	get_member_worker(cls, Instantiator::nil(), member_name)
}

pub fn try_get_member_of_inst_cls(cls: &InstCls, member_name: Sym) -> Option<InstMember> {
	get_member_worker(cls.class(), Instantiator::of_inst_cls(cls), member_name)
}

fn get_member_worker(
	cls: &ClassDeclaration,
	instantiator: Instantiator,
	member_name: Sym,
) -> Option<InstMember> {
	for method in cls.methods.iter() {
		if method.name() == member_name {
			return Some(InstMember(MemberDeclaration::Method(method.ptr()), instantiator))
		}
	}

	match *cls.head {
		ClassHead::Static | ClassHead::Builtin => {}
		ClassHead::Slots(_, ref slots) =>
			for slot in slots.iter() {
				if slot.name == member_name {
					return Some(InstMember(MemberDeclaration::Slot(slot.ptr()), instantiator))
				}
			},
		ClassHead::Abstract(_, ref methods) =>
			for method in methods.iter() {
				if method.name() == member_name {
					return Some(InstMember(MemberDeclaration::AbstractMethod(method.ptr()), instantiator))
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

pub struct InstMember(pub MemberDeclaration, pub Instantiator);
