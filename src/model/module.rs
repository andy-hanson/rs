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
	Builtin(Sym),
}
impl ModuleSourceEnum {
	pub fn assert_normal(&self) -> &ModuleSource {
		match *self {
			ModuleSourceEnum::Normal(ref source) => source,
			ModuleSourceEnum::Builtin(_) => unreachable!(),
		}
	}
}

pub struct ModuleSource {
	pub logical_path: Ptr<Path>,
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

	pub fn name(&self) -> Sym {
		match *self {
			OwnModuleOrFail::Module(ref m) => m.name(),
			OwnModuleOrFail::Fail(ref f) => f.name(),
		}
	}

	pub fn source(&self) -> &ModuleSourceEnum {
		match *self {
			OwnModuleOrFail::Module(ref m) => &m.source,
			OwnModuleOrFail::Fail(ref f) => &f.source,
		}
	}

	pub fn diagnostics(&self) -> &[Diagnostic] {
		match *self {
			OwnModuleOrFail::Module(ref m) => &m.diagnostics,
			OwnModuleOrFail::Fail(ref f) => &f.diagnostics,
		}
	}
}

pub enum PtrModuleOrFail {
	Module(Ptr<Module>),
	Fail(Ptr<FailModule>),
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
