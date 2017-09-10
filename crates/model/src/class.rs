use serde::{Serialize, Serializer};

use util::arena::NoDrop;
use util::arr::SliceOps;
use util::late::Late;
use util::loc::Loc;
use util::sym::Sym;
use util::up::SerializeUp;

use super::method::{AbstractMethod, Impl, MethodWithBody};

use super::ty::{InstCls, Ty, TypeParameter};

#[derive(Serialize)]
pub struct ClassDeclaration<'a> {
	pub name: Sym,
	pub type_parameters: &'a [TypeParameter<'a>],
	pub head: Late<ClassHead<'a>>,
	pub supers: Late<&'a [Super<'a>]>,
	// Abstract methods are stored in the `head`
	//TODO:PERF would like an array of methods, not references to methods
	pub methods: Late<&'a [&'a MethodWithBody<'a>]>,
}
impl<'a> NoDrop for ClassDeclaration<'a> {}
impl<'a> ClassDeclaration<'a> {
	//mv
	pub fn find_static_method(&self, name: Sym) -> Option<&'a MethodWithBody<'a>> {
		self.methods
			.find(|m| m.is_static && m.name() == name)
			.map(|m| *m)
	}
}
impl<'a> SerializeUp for ClassDeclaration<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub enum ClassHead<'a> {
	Static,
	Abstract(Loc, &'a [AbstractMethod<'a>]),
	Slots(Loc, &'a [SlotDeclaration<'a>]),
	// Implementation details are completely hidden.
	Builtin,
}
impl<'a> NoDrop for ClassHead<'a> {}

#[derive(Serialize)]
pub struct SlotDeclaration<'a> {
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for SlotDeclaration<'a> {}
impl<'a> SerializeUp for SlotDeclaration<'a> {
	fn serialize_up<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Super<'a> {
	pub loc: Loc,
	pub super_class: InstCls<'a>,
	pub impls: &'a [Impl<'a>],
}
impl<'a> NoDrop for Super<'a> {}

#[derive(Copy, Clone)]
pub enum MemberDeclaration<'a> {
	Slot(&'a SlotDeclaration<'a>),
	Method(&'a MethodWithBody<'a>),
	AbstractMethod(&'a AbstractMethod<'a>),
}
impl<'a> NoDrop for MemberDeclaration<'a> {}
impl<'a> MemberDeclaration<'a> {
	pub fn name(&self) -> Sym {
		match *self {
			MemberDeclaration::Slot(s) => s.name,
			MemberDeclaration::Method(m) => m.name(),
			MemberDeclaration::AbstractMethod(a) => a.name(),
		}
	}
}
impl<'a> Serialize for MemberDeclaration<'a> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let name = match *self {
			MemberDeclaration::Slot(s) => s.name,
			MemberDeclaration::Method(m) => m.name(),
			MemberDeclaration::AbstractMethod(a) => a.name(),
		};
		name.serialize(serializer)
	}
}
