use util::arr::Arr;
use util::loc::Loc;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::method::{AbstractMethod, MethodWithBody, Impl};
//use super::module::Module;

use super::ty::{InstCls, Ty, TypeParameter};

pub struct ClassDeclaration {
	//_module: LatePtr<Module>,
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	_head: LateOwn<ClassHead>,
	_supers: LateOwn<Arr<Super>>,
	// Abstract methods are stored in the `head`
	_methods: LateOwn<Arr<Own<MethodWithBody>>>,
}
impl ClassDeclaration {
	pub fn new(name: Sym, type_parameters: Arr<Own<TypeParameter>>) -> ClassDeclaration {
		ClassDeclaration {
			//_module: LatePtr::new(),
			type_parameters,
			name,
			_head: LateOwn::new(),
			_supers: LateOwn::new(),
			_methods: LateOwn::new(),
		}
	}

	//fn module(&self) -> &Module {
	//	&self._module
	//}

	pub fn head(&self) -> &ClassHead {
		&self._head
	}
	pub fn set_head(&self, head: ClassHead) {
		self._head.init(head)
	}
	pub fn supers(&self) -> &Arr<Super> {
		&self._supers
	}
	pub fn set_supers(&self, supers: Arr<Super>) {
		self._supers.init(supers)
	}
	pub fn methods(&self) -> &Arr<Own<MethodWithBody>> {
		&self._methods
	}
	pub fn set_methods(&self, methods: Arr<Own<MethodWithBody>>) {
		self._methods.init(methods)
	}

	pub fn find_static_method(&self, name: Sym) -> Option<&Own<MethodWithBody>> {
		self.methods().find(|m| m.is_static && m.name() == name)
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
impl SlotDeclaration {
	pub fn of(loc: Loc, mutable: bool, ty: Ty, name: Sym) -> SlotDeclaration {
		SlotDeclaration { loc, mutable, ty, name }
	}
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
