use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::method::{AbstractMethod, Impl, MethodWithBody};
//use super::module::Module;

use super::ty::{InstCls, Ty, TypeParameter};

pub struct ClassDeclaration {
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	pub head: LateOwn<ClassHead>,
	pub supers: LateOwn<Arr<Super>>,
	// Abstract methods are stored in the `head`
	pub methods: LateOwn<Arr<Own<MethodWithBody>>>,
}
impl ClassDeclaration {
	//mv
	pub fn find_static_method(&self, name: Sym) -> Option<&Own<MethodWithBody>> {
		self.methods.find(|m| m.is_static && m.name() == name)
	}
}

pub enum ClassHead {
	Static,
	Abstract(Loc, Arr<Own<AbstractMethod>>),
	Slots(Loc, Arr<Own<SlotDeclaration>>),
	// Implementation details are completely hidden.
	Builtin,
}

pub struct SlotDeclaration {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty,
	pub name: Sym,
}

pub struct Super {
	pub loc: Loc,
	pub super_class: InstCls,
	pub impls: Arr<Own<Impl>>,
}

pub enum MemberDeclaration {
	Slot(Ptr<SlotDeclaration>),
	Method(Ptr<MethodWithBody>),
	AbstractMethod(Ptr<AbstractMethod>),
}
