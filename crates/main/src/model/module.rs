use util::arr::Arr;
use util::path::Path;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::compiler::full_path;
use super::super::host::document_info::DocumentInfo;

use super::class::ClassDeclaration;
use super::diag::Diagnostic;

pub enum ModuleSourceEnum {
	Normal(ModuleSource),
	Builtin { name: Sym, text: &'static [u8] },
}
impl ModuleSourceEnum {
	pub fn assert_normal(&self) -> &ModuleSource {
		match *self {
			ModuleSourceEnum::Normal(ref source) => source,
			ModuleSourceEnum::Builtin { .. } => unreachable!(),
		}
	}

	pub fn text(&self) -> &[u8] {
		match *self {
			ModuleSourceEnum::Normal(ref source) => &source.document.text,
			ModuleSourceEnum::Builtin { text, .. } => text,
		}
	}
}

pub struct ModuleSource {
	pub logical_path: Path,
	pub is_index: bool,
	pub document: DocumentInfo,
}
impl ModuleSource {
	pub fn full_path(&self) -> Path {
		full_path(&self.logical_path, self.is_index)
	}
}

pub enum OwnModuleOrFail {
	Module(Own<Module>),
	Fail(Own<FailModule>),
}
impl OwnModuleOrFail {
	pub fn to_ptr(&self) -> PtrModuleOrFail {
		match *self {
			OwnModuleOrFail::Module(ref m) => PtrModuleOrFail::Module(m.ptr()),
			OwnModuleOrFail::Fail(ref f) => PtrModuleOrFail::Fail(f.ptr()),
		}
	}

	fn to_ref(&self) -> RefModuleOrFail {
		match *self {
			OwnModuleOrFail::Module(ref m) => RefModuleOrFail::Module(m),
			OwnModuleOrFail::Fail(ref f) => RefModuleOrFail::Fail(f),
		}
	}

	pub fn source(&self) -> &ModuleSourceEnum {
		self.to_ref().source()
	}

	pub fn diagnostics(&self) -> &[Diagnostic] {
		self.to_ref().diagnostics()
	}
}

pub enum PtrModuleOrFail {
	Module(Ptr<Module>),
	Fail(Ptr<FailModule>),
}
impl PtrModuleOrFail {
	fn to_ref(&self) -> RefModuleOrFail {
		match *self {
			PtrModuleOrFail::Module(ref m) => RefModuleOrFail::Module(m),
			PtrModuleOrFail::Fail(ref f) => RefModuleOrFail::Fail(f),
		}
	}

	pub fn name(&self) -> Sym {
		self.to_ref().name()
	}
	pub fn source(&self) -> &ModuleSourceEnum {
		self.to_ref().source()
	}
	pub fn diagnostics(&self) -> &[Diagnostic] {
		self.to_ref().diagnostics()
	}
}

enum RefModuleOrFail<'a> {
	Module(&'a Module),
	Fail(&'a FailModule),
}
impl<'a> RefModuleOrFail<'a> {
	pub fn name(self) -> Sym {
		match self {
			RefModuleOrFail::Module(m) => m.name(),
			RefModuleOrFail::Fail(f) => f.name(),
		}
	}

	pub fn source(self) -> &'a ModuleSourceEnum {
		match self {
			RefModuleOrFail::Module(m) => &m.source,
			RefModuleOrFail::Fail(f) => &f.source,
		}
	}

	pub fn diagnostics(self) -> &'a [Diagnostic] {
		match self {
			RefModuleOrFail::Module(m) => &m.diagnostics,
			RefModuleOrFail::Fail(f) => &f.diagnostics,
		}
	}
}

pub struct FailModule {
	pub source: ModuleSourceEnum,
	pub imports: Arr<PtrModuleOrFail>,
	pub diagnostics: Arr<Diagnostic>,
}
impl FailModule {
	fn name(&self) -> Sym {
		unimplemented!()
	}
}

pub struct Module {
	pub source: ModuleSourceEnum,
	pub imports: Arr<Ptr<Module>>,
	pub class: LateOwn<ClassDeclaration>,
	pub diagnostics: LateOwn<Arr<Diagnostic>>,
}
impl Module {
	pub fn name(&self) -> Sym {
		self.class.name
	}

	pub fn class(&self) -> Ptr<ClassDeclaration> {
		self.class.ptr()
	}
}
