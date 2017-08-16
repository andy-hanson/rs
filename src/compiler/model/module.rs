use compiler::diag::Diagnostic;
use compiler::host::document_info::DocumentInfo;
use compiler::module_resolver::full_path;

use util::arr::Arr;
use util::path::Path;
use util::ptr::{ Own, Ptr, LateOwn };
use util::sym::Sym;

use super::class_declaration::ClassDeclaration;

pub enum Imported {
	// Not Weak because we want our dependencies to be kept alive.
	// They can't have pointers back to us.
	Module(Ptr<Module>),
	Builtin(Ptr<ClassDeclaration>),
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
