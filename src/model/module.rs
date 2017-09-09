use util::arena::{Arena, List, NoDrop, Up};
use util::late::Late;
use util::path::Path;
use util::sym::Sym;

use super::super::compiler::full_path;
use super::super::host::document_info::DocumentInfo;

use super::class::ClassDeclaration;
use super::diag::Diagnostic;

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

pub struct ModuleSource<'a> {
	pub logical_path: Path<'a>, //TODO:Memory leak!
	pub is_index: bool,
	pub document: DocumentInfo<'a>,
}
impl<'a> NoDrop for ModuleSource<'a> {}
impl<'a> ModuleSource<'a> {
	pub fn full_path<'out>(&self, arena: &'out Arena) -> Path<'out> {
		full_path(&self.logical_path, self.is_index, arena)
	}
}

pub enum ModuleOrFail<'a> {
	Module(&'a Module<'a>),
	Fail(&'a FailModule<'a>),
}
impl<'a> NoDrop for ModuleOrFail<'a> {}
impl<'a> ModuleOrFail<'a> {
	//TODO: just derive Copy
	pub fn clone_as_ptr(&self) -> Self {
		match *self {
			ModuleOrFail::Module(m) => ModuleOrFail::Module(m),
			ModuleOrFail::Fail(f) => ModuleOrFail::Fail(f),
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

	pub fn diagnostics(&self) -> &'a List<'a, Diagnostic<'a>> {
		match *self {
			ModuleOrFail::Module(m) => &*m.diagnostics,
			ModuleOrFail::Fail(f) => &f.diagnostics,
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
