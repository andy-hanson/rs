use util::arena::{Arena, NoDrop};
use util::late::Late;
use util::list::List;
use util::path::Path;
use util::show::{Show, Shower};
use util::sym::Sym;
use util::up::Up;

use super::class::ClassDeclaration;
use super::diag::Diagnostic;
use super::document_info::DocumentInfo;

pub enum ModuleSourceEnum<'a> {
	Normal(ModuleSource<'a>),
	Builtin { name: Sym, text: &'static [u8] },
}
impl<'a> NoDrop for ModuleSourceEnum<'a> {}
impl<'a> ModuleSourceEnum<'a> {
	pub fn assert_normal(&self) -> &ModuleSource<'a> {
		match *self {
			ModuleSourceEnum::Normal(ref source) => source,
			ModuleSourceEnum::Builtin { .. } => unreachable!(),
		}
	}

	pub fn text(&self) -> &[u8] {
		match *self {
			ModuleSourceEnum::Normal(ref source) => source.document.text,
			ModuleSourceEnum::Builtin { text, .. } => text,
		}
	}
}
impl<'m, 'a> Show for &'m ModuleSourceEnum<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			ModuleSourceEnum::Normal(ref ms) => {
				s.add('\'')?.add(ms.full_path)?.add('\'')?;
			}
			ModuleSourceEnum::Builtin { name, .. } => {
				s.add("Builtin ")?.add(name)?;
			}
		}
		Ok(())
	}
}

pub struct ModuleSource<'a> {
	pub logical_path: Path<'a>, //This is a slice of full_path
	pub full_path: Path<'a>,
	pub is_index: bool,
	pub document: DocumentInfo<'a>,
}
impl<'a> NoDrop for ModuleSource<'a> {}

#[derive(Copy, Clone)]
pub enum ModuleOrFail<'a> {
	Module(&'a Module<'a>),
	Fail(&'a FailModule<'a>),
}
impl<'a> NoDrop for ModuleOrFail<'a> {}
impl<'a> ModuleOrFail<'a> {
	pub fn assert_success(&self) -> &'a Module<'a> {
		match *self {
			ModuleOrFail::Module(m) => m,
			_ => unreachable!(),
		}
	}

	pub fn copy_to_new_arena<'out>(&self, arena: &'out Arena) -> ModuleOrFail<'out> {
		unused!(arena);
		unimplemented!()
	}

	pub fn name(self) -> Sym {
		match self {
			ModuleOrFail::Module(m) => m.name(),
			ModuleOrFail::Fail(f) => f.name(),
		}
	}

	pub fn source(&self) -> &'a ModuleSourceEnum<'a> {
		match *self {
			ModuleOrFail::Module(m) => &m.source,
			ModuleOrFail::Fail(f) => &f.source,
		}
	}

	pub fn diagnostics(&self) -> List<'a, Diagnostic<'a>> {
		match *self {
			ModuleOrFail::Module(m) => *m.diagnostics,
			ModuleOrFail::Fail(f) => f.diagnostics,
		}
	}
}

pub struct FailModule<'a> {
	pub source: ModuleSourceEnum<'a>,
	pub imports: &'a [ModuleOrFail<'a>],
	pub diagnostics: List<'a, Diagnostic<'a>>,
}
impl<'a> NoDrop for FailModule<'a> {}
impl<'a> FailModule<'a> {
	fn name(&self) -> Sym {
		unimplemented!()
	}
}

pub struct Module<'a> {
	pub source: ModuleSourceEnum<'a>,
	pub imports: &'a [Up<'a, Module<'a>>],
	pub class: Late<ClassDeclaration<'a>>,
	pub diagnostics: Late<List<'a, Diagnostic<'a>>>,
}
impl<'a> NoDrop for Module<'a> {}
impl<'a> Module<'a> {
	pub fn name(&self) -> Sym {
		self.class.name
	}
}
