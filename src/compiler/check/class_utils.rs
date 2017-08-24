use compiler::model::class::{ClassDeclaration, ClassHead, MemberDeclaration};
use compiler::model::ty::InstCls;

use util::sym::Sym;

use super::ty_replacer::TyReplacer;


pub fn try_get_member_from_class_declaration(
	cls: &ClassDeclaration,
	member_name: Sym,
) -> Option<InstMember> {
	get_member_worker(cls, TyReplacer::do_nothing(), member_name)
}

pub fn try_get_member_of_inst_cls(cls: &InstCls, member_name: Sym) -> Option<InstMember> {
	get_member_worker(cls.class(), TyReplacer::of_inst_cls(cls), member_name)
}

fn get_member_worker(
	cls: &ClassDeclaration,
	replacer: TyReplacer,
	member_name: Sym,
) -> Option<InstMember> {
	for method in cls.methods().iter() {
		if method.name() == member_name {
			return Some(InstMember(MemberDeclaration::Method(method.ptr()), replacer));
		}
	}

	match *cls.head() {
		ClassHead::Static | ClassHead::Builtin => {}
		ClassHead::Slots(_, ref slots) => {
			for slot in slots.iter() {
				if slot.name == member_name {
					return Some(InstMember(MemberDeclaration::Slot(slot.ptr()), replacer));
				}
			}
		}
		ClassHead::Abstract(_, ref methods) => {
			for method in methods.iter() {
				if method.name() == member_name {
					return Some(
						InstMember(MemberDeclaration::AbstractMethod(method.ptr()), replacer),
					);
				}
			}
		}
	}

	for zuper in cls.supers().iter() {
		let super_replacer = replacer.combine(&TyReplacer::of_inst_cls(&zuper.super_class));
		let got = get_member_worker(zuper.super_class.class(), super_replacer, member_name);
		if got.is_some() {
			return got;
		}
	}

	None
}

pub struct InstMember(pub MemberDeclaration, pub TyReplacer);
