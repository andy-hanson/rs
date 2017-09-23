use serde::{Serialize, Serializer};

use util::arena::{Arena, NoDrop};
use util::iter::{slice_is_empty, KnownLen};
use util::late::Late;
use util::loc::Loc;
use util::show::{Show, Shower};
use util::sym::Sym;
use util::up::{SerializeUp, Up};

use super::method::{AbstractMethod, Impl, MethodWithBody};

use super::ty::{Ty, TypeParameter};

#[derive(Serialize)]
pub struct ClassDeclaration<'a> {
	pub name: Sym,
	#[serde(skip_serializing_if = "slice_is_empty")]
	pub type_parameters: &'a [TypeParameter<'a>],
	pub head: Late<ClassHead<'a>>,
	#[serde(skip_serializing_if = "slice_is_empty")]
	pub supers: Late<&'a [Super<'a>]>,
	// Abstract methods are stored in the `head`
	pub methods: Late<&'a [MethodWithBody<'a>]>,
}
impl<'a> NoDrop for ClassDeclaration<'a> {}
impl<'a> ClassDeclaration<'a> {
	pub fn find_slot(&self, name: Sym) -> Option<Up<'a, SlotDeclaration<'a>>> {
		match *self.head {
			ClassHead::Slots(SlotsData { loc: _, ref slots }) =>
				slots.iter().find(|s| s.name == name).map(Up),
			_ => None,
		}
	}

	//mv
	pub fn find_static_method(&self, name: Sym) -> Option<Up<'a, MethodWithBody<'a>>> {
		self.methods
			.iter()
			.find(|m| m.is_static && m.name() == name)
			.map(Up)
	}

	pub fn all_impls(&self) -> impl Iterator<Item = &'a Impl<'a>> {
		self.supers.iter().flat_map(|s: &'a Super<'a>| s.impls)
	}
}

impl<'a> SerializeUp for ClassDeclaration<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub enum ClassHead<'a> {
	Static,
	Abstract(Loc, &'a [AbstractMethod<'a>]),
	Slots(SlotsData<'a>),
	// Implementation details are completely hidden.
	Builtin,
}
impl<'a> NoDrop for ClassHead<'a> {}

#[derive(Serialize)]
pub struct SlotsData<'a> {
	pub loc: Loc,
	pub slots: Late<&'a [SlotDeclaration<'a>]>,
}

#[derive(Serialize)]
pub struct SlotDeclaration<'a> {
	#[serde(skip_serializing)]
	pub slots: Up<'a, SlotsData<'a>>,
	pub loc: Loc,
	pub mutable: bool,
	pub ty: Ty<'a>,
	pub name: Sym,
}
impl<'a> NoDrop for SlotDeclaration<'a> {}
impl<'a> SerializeUp for SlotDeclaration<'a> {
	fn serialize_up<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		self.name.serialize(serializer)
	}
}

#[derive(Serialize)]
pub struct Super<'a> {
	pub loc: Loc,
	pub super_class: InstClass<'a>,
	pub impls: &'a [Impl<'a>],
}
impl<'a> NoDrop for Super<'a> {}

#[derive(Clone, Hash, Serialize)]
pub struct InstClass<'a> {
	pub class: Up<'a, ClassDeclaration<'a>>,
	pub ty_args: &'a [Late<Ty<'a>>],
}
impl<'a> InstClass<'a> {
	pub fn generic_self_reference(class: Up<'a, ClassDeclaration<'a>>, arena: &'a Arena) -> Self {
		let ty_args = arena.map(class.type_parameters, |tp| Late::full(Ty::Param(Up(tp))));
		InstClass { class, ty_args }
	}

	pub fn fast_equals(&self, other: &Self) -> bool {
		self.class.ptr_eq(other.class)
			&& self.ty_args
				.each_equals(other.ty_args, |a, b| a.fast_equals(&*b))
	}
}
impl<'a> Eq for InstClass<'a> {}
impl<'a> PartialEq for InstClass<'a> {
	fn eq(&self, other: &InstClass<'a>) -> bool {
		self.fast_equals(other)
	}
}
impl<'a> NoDrop for InstClass<'a> {}
impl<'i, 'a> Show for &'i InstClass<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		s.add(self.class.name)?;
		if !self.ty_args.is_empty() {
			s.add('[')?;
			s.join(self.ty_args)?;
			s.add(']')?;
		}
		Ok(())
	}
}
