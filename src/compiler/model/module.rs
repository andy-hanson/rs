use compiler::diag::Diagnostic;
use compiler::host::document_info::DocumentInfo;
use compiler::module_resolver::full_path;

use util::arr::Arr;
use util::path::Path;
use util::ptr::{ Own, Ptr, LateOwn };
use util::sym::Sym;

use super::class::ClassDeclaration;

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

pub struct Module {
	logical_path: Path,
	is_index: bool,
	document: Own<DocumentInfo>, //TODO: should own this?
	imports: Arr<Imported>,
	class: LateOwn<ClassDeclaration>,
	diagnostics: LateOwn<Arr<Diagnostic>>,
}
impl Module {
	pub fn new(logical_path: Path, is_index: bool, document: Own<DocumentInfo>, imports: Arr<Imported>) -> Module {
		Module {
			logical_path, is_index, document, imports,
			class: LateOwn::new(),
			diagnostics: LateOwn::new(),
		}
	}

	pub fn full_path(&self) -> Path {
		full_path(&self.logical_path, self.is_index)
	}

	pub fn name(&self) -> Sym {
		self.class.name
	}
}
