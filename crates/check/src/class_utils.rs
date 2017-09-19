use util::sym::Sym;

use model::class::{ClassDeclaration, ClassHead, InstClass, MemberDeclaration};

use super::instantiator::Instantiator;

pub fn try_get_member_from_class_declaration<'a>(
	class: &ClassDeclaration<'a>,
	member_name: Sym,
) -> Option<InstMember<'a>> {
	get_member_worker(class, Instantiator::NIL, member_name)
}

pub fn try_get_member_of_inst_class<'a>(class: &InstClass<'a>, member_name: Sym) -> Option<InstMember<'a>> {
	get_member_worker(&class.class, Instantiator::of_inst_class(class), member_name)
}

fn get_member_worker<'a>(
	class: &ClassDeclaration<'a>,
	instantiator: Instantiator<'a>,
	member_name: Sym,
) -> Option<InstMember<'a>> {
	for method in *class.methods {
		if method.name() == member_name {
			return Some(InstMember(MemberDeclaration::Method(method), instantiator))
		}
	}

	match *class.head {
		ClassHead::Static | ClassHead::Builtin => {}
		ClassHead::Slots(_, slots) =>
			for slot in slots {
				if slot.name == member_name {
					return Some(InstMember(MemberDeclaration::Slot(slot), instantiator))
				}
			},
		ClassHead::Abstract(_, methods) =>
			for method in methods {
				if method.name() == member_name {
					return Some(InstMember(MemberDeclaration::AbstractMethod(method), instantiator))
				}
			},
	}

	for zuper in *class.supers {
		let super_instantiator = instantiator.combine(&Instantiator::of_inst_class(&zuper.super_class));
		let got = get_member_worker(&zuper.super_class.class, super_instantiator, member_name);
		if got.is_some() {
			return got
		}
	}

	None
}

pub struct InstMember<'a>(pub MemberDeclaration<'a>, pub Instantiator<'a>);
