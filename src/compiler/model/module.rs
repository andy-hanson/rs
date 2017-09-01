use util::arr::Arr;
use util::path::Path;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::compile::full_path;
use super::super::diag::Diagnostic;

use super::class::ClassDeclaration;

pub struct ModuleSource {
	pub logical_path: Ptr<Path>,
	pub is_index: bool,
	pub document_version: u32,
	//pub document: DocumentInfo,
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

	pub fn source(&self) -> Option<&ModuleSource> {
		match *self {
			OwnModuleOrFail::Module(ref m) => m.source.as_ref(),
			OwnModuleOrFail::Fail(ref f) => f.source.as_ref(),
		}
	}
}

pub enum PtrModuleOrFail {
	Module(Ptr<Module>),
	Fail(Ptr<FailModule>),
}

pub struct FailModule {
	pub source: Option<ModuleSource>,
	pub imports: Arr<PtrModuleOrFail>,
	pub diagnostics: Arr<Diagnostic>,
}

pub struct Module {
	pub source: Option<ModuleSource>, // missing for builtins
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

/*
pub enum Imported {
	// Not Weak because we want our dependencies to be kept alive.
	// They can't have pointers back to us.
	Module(Ptr<Module>),
	Builtin(Ptr<ClassDeclaration>),
}
impl Imported {
	pub fn name(&self) -> Sym {
		match *self {
			Imported::Module(ref m) => m.name(),
			Imported::Builtin(ref c) => c.name,
		}
	}

	pub fn imported_class(&self) -> Ptr<ClassDeclaration> {
		match *self {
			Imported::Module(ref m) => m.class.ptr(),
			Imported::Builtin(ref p) => p.clone_ptr(),
		}
	}
}
*/
