use serde::{Serialize, Serializer};

use util::arr::{Arr, SliceOps};
use util::loc::Loc;
use util::ptr::{LateOwn, Own, Ptr, SerializeAsPtr};
use util::sym::Sym;

use super::method::{AbstractMethod, Impl, MethodWithBody};
//use super::module::Module;

use super::ty::{InstCls, Ty, TypeParameter};

#[derive(Serialize)]
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
impl SerializeAsPtr for ClassDeclaration {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub enum ClassHead {
	Static,
	Abstract(Loc, Arr<Own<AbstractMethod>>),
	Slots(Loc, Arr<Own<SlotDeclaration>>),
	// Implementation details are completely hidden.
	Builtin,
}

#[derive(Serialize)]
pub struct SlotDeclaration {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty,
	pub name: Sym,
}
impl SerializeAsPtr for SlotDeclaration {
	fn serialize_as_ptr<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
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
impl Serialize for MemberDeclaration {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let name = match *self {
			MemberDeclaration::Slot(ref s) => s.name,
			MemberDeclaration::Method(ref m) => m.name(),
			MemberDeclaration::AbstractMethod(ref a) => a.name(),
		};
		name.serialize(serializer)
	}
}
