use util::arr::{Arr, ArrBuilder};
use util::dict::MutDict;
use util::path::Path;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::check::check_class;
use super::diag::{Diag, Diagnostic};
use super::host::document_info::DocumentInfo;
use super::host::document_provider::DocumentProvider;
use super::host::file_input::Result;
use super::model::module::{FailModule, Module, ModuleCommon, OwnModuleOrFail, PtrModuleOrFail};
use super::module_resolver::{get_document_from_logical_path, GetDocumentResult};
use super::parse::ast::{Class as AstClass, Import as AstImport, Module as AstModule};

pub struct CompiledProgram(MutDict<Own<Path>, OwnModuleOrFail>);

/*
Design notes:

We assume that there may be file system events that we don't know about.
Therefore, we will always ask the CompileHost for the latest versions of all files.
The CompilerHost may implement caching. (In a command-line scenario this should not be necessary.)
Whether a document may be reused is indicated by its version vs the version the compiler used.
*/
struct Compiler<'a> {
	document_provider: &'a DocumentProvider,
	// We consume the old program, so we module values out of its map when we reuse them.
	old_program: Option<CompiledProgram>,
	// Keys are logical paths.
	modules: MutDict<Own<Path>, ModuleState>,
}
impl<'a> Compiler<'a> {
	fn new(document_provider: &'a DocumentProvider, old_program: Option<CompiledProgram>) -> Compiler<'a> {
		Compiler { document_provider, old_program, modules: MutDict::new() }
	}

	fn compile_single(&mut self, logical_path: Path) -> Result<(CompileSingleResult, bool)> {
		if let Some(already_compiled) = self.modules.get(&logical_path) {
			return Ok(match *already_compiled {
				ModuleState::Compiling =>
					// TODO: attach an error to the calling module
					(CompileSingleResult::Circular, false),
				ModuleState::CompiledFresh(ref module) =>
					(CompileSingleResult::Found(module.to_ptr()), false),
				ModuleState::CompiledReused(ref module) =>
					// Already compiled in the new program.
					// This can happen if the same module is a dependency of two other modules.
					(CompileSingleResult::Found(module.to_ptr()), true),
			})
		}

		match get_document_from_logical_path(self.document_provider, &logical_path)? {
			GetDocumentResult::Found { full_path, is_index, document } =>
				self.compile_module_from_document(logical_path, full_path, is_index, document),
			GetDocumentResult::NotFound => Ok((CompileSingleResult::Missing, false)),
		}
	}

	fn compile_module_from_document(
		&mut self,
		logical_path: Path,
		full_path: Path,
		is_index: bool,
		document: DocumentInfo,
	) -> Result<(CompileSingleResult, bool)> {
		let own_logical_path = Own::new(logical_path);
		Ok(match document.parse_result {
			Ok(AstModule { imports, class }) => {
				let ptr_logical_path = own_logical_path.ptr();
				self.modules.add(own_logical_path, ModuleState::Compiling);
				let (module, is_reused) = self.do_compile_single(
					&ptr_logical_path,
					document.version,
					imports,
					*class,
					full_path,
					is_index,
				)?;
				let module_ptr = module.to_ptr();
				let module_state = if is_reused {
					ModuleState::CompiledReused(module)
				} else {
					ModuleState::CompiledFresh(module)
				};
				self.modules.change(&*ptr_logical_path, module_state);
				(CompileSingleResult::Found(module_ptr), is_reused)
			}
			Err(parse_diag) => {
				let common = ModuleCommon {
					logical_path: own_logical_path.ptr(),
					is_index,
					document_version: document.version,
				};
				let diagnostics = Arr::_1(parse_diag);
				let fail = Own::new(FailModule { common, imports: Arr::empty(), diagnostics });
				let fail_ptr = fail.ptr();
				self.modules
					.add(own_logical_path, ModuleState::CompiledFresh(OwnModuleOrFail::Fail(fail)));
				(CompileSingleResult::Found(PtrModuleOrFail::Fail(fail_ptr)), false)
			}
		})
	}

	fn do_compile_single(
		&mut self,
		logical_path: &Ptr<Path>,
		document_version: u32,
		import_asts: Arr<AstImport>,
		class_ast: AstClass,
		full_path: Path,
		is_index: bool,
	) -> Result<(OwnModuleOrFail, bool)> {
		let (resolved_imports, all_dependencies_reused) = self.resolve_imports(&full_path, import_asts)?;

		// We will only bother looking at the old module if all of our dependencies were safely reused.
		// If oldModule doesn't exactly match, we'll ignore it completely.
		if all_dependencies_reused {
			if let Some(ref mut old_program) = self.old_program {
				if let Some(old_module_or_fail) = old_program.0.try_extract(&**logical_path) {
					if old_module_or_fail.common().document_version == document_version {
						return Ok((old_module_or_fail, true))
					}
				}
			}
		}

		let common = ModuleCommon { logical_path: logical_path.clone_ptr(), is_index, document_version };
		let res = match resolved_imports {
			ResolvedImports::Success(imports) => {
				let module =
					Own::new(Module { common, imports, class: LateOwn::new(), diagnostics: LateOwn::new() });
				let name = match logical_path.last() {
					Some(name) => Sym::from_slice(name.as_slice()),
					None => self.document_provider.root_name(),
				};
				// Initializes module.class
				let diagnostics = check_class(&module.class, &module.imports, &class_ast, name);
				// We only type-check if there were no parse/import diagnostics,
				// so if we got here, these are the only diagnostics.
				module.diagnostics.init(diagnostics);
				OwnModuleOrFail::Module(module)
			}
			ResolvedImports::Failure(imports, diagnostics) => {
				let fail = Own::new(FailModule { common, imports, diagnostics });
				OwnModuleOrFail::Fail(fail)
			}
		};
		Ok((res, false))
	}

	fn resolve_imports(
		&mut self,
		full_path: &Path,
		import_asts: Arr<AstImport>,
	) -> Result<(ResolvedImports, bool)> {
		let mut diagnostics = ArrBuilder::<Diagnostic>::new();
		let mut all_imports_reused = true;
		//TODO:PERF probably, all imports will succeed, so allocate whole array.
		let mut all_successes = ArrBuilder::<Ptr<Module>>::new();
		//TODO:PERF probably, this will not be necessary, somehow avoid allocating?
		let mut all = ArrBuilder::<PtrModuleOrFail>::new();
		let mut any_failure = false;
		let import_asts_len = import_asts.len();
		for import_ast in import_asts.move_into_iter() {
			match self.resolve_import(import_ast, &mut diagnostics, full_path, &mut all_imports_reused)? {
				Some(import) => {
					if let PtrModuleOrFail::Module(ref module) = import {
						all_successes.add(module.clone_ptr())
					} else {
						any_failure = true
					}
					all.add(import)
				}
				None => any_failure = true,
			}
		}

		let resolved = if any_failure {
			ResolvedImports::Failure(all.finish(), diagnostics.finish())
		} else {
			assert_eq!(diagnostics.len(), 0);
			assert_eq!(all_successes.len(), import_asts_len);
			ResolvedImports::Success(all_successes.finish())
		};
		Ok((resolved, all_imports_reused))
	}

	fn resolve_import(
		&mut self,
		import_ast: AstImport,
		import_diagnostics: &mut ArrBuilder<Diagnostic>,
		full_path: &Path,
		all_dependencies_reused: &mut bool,
	) -> Result<Option<PtrModuleOrFail>> {
		match import_ast {
			AstImport::Global(_loc, _path) => {
				todo!() // Get the builtin with this name
			}
			AstImport::Local(loc, relative_path) => {
				let (imported_module, is_import_reused) =
					self.compile_single(full_path.resolve(&relative_path))?;
				*all_dependencies_reused &= is_import_reused;
				Ok(match imported_module {
					CompileSingleResult::Circular => {
						import_diagnostics.add(
							Diagnostic(loc, Diag::CircularDependency(full_path.clone_path(), relative_path)),
						);
						None
					}
					CompileSingleResult::Missing => {
						import_diagnostics.add(
							Diagnostic(loc, Diag::CantFindLocalModule(full_path.clone_path(), relative_path)),
						);
						None
					}
					CompileSingleResult::Found(found) => Some(found),
				})
			}
		}
	}
}

enum ResolvedImports {
	Success(Arr<Ptr<Module>>),
	Failure(Arr<PtrModuleOrFail>, Arr<Diagnostic>),
}


pub fn compile(
	path: Path,
	document_provider: &DocumentProvider,
	old_program: Option<CompiledProgram>,
) -> Result<CompileResult> {
	let mut compiler = Compiler::new(document_provider, old_program);
	// Don't care about isReused for top level
	Ok(match compiler.compile_single(path)?.0 {
		CompileSingleResult::Found(root) => {
			let modules = compiler.modules.map_values(|o| match o {
					ModuleState::CompiledFresh(m) | ModuleState::CompiledReused(m) => m,
					ModuleState::Compiling => unreachable!(),
				});
			let new_program = CompiledProgram(modules);
			CompileResult::RootFound(new_program, root)
		}
		CompileSingleResult::Missing =>
			CompileResult::RootMissing,
		CompileSingleResult::Circular =>
			// Very first module can't be circular
			unreachable!(),
	})
}

pub enum CompileResult {
	RootMissing,
	RootFound(CompiledProgram, PtrModuleOrFail),
}

//mv to a different module?
pub fn compile_dir(dir: Path) -> Result<CompileResult> {
	compile(Path::empty(), &DocumentProvider::file_system(dir), None)
}
pub fn compile_file(path: &Path) -> Result<CompileResult> {
	let file_name = path.last().unwrap().without_end_if_ends_with(".nz"); //TODO: magic constant
	let file_path = if file_name.equals_str("index") {
		Path::empty()
	} else {
		Path::from_string(file_name)
	};
	compile(file_path, &DocumentProvider::file_system(path.directory()), None)
}

enum CompileSingleResult {
	Missing,
	Circular,
	Found(PtrModuleOrFail),
}

enum ModuleState {
	Compiling,
	CompiledFresh(OwnModuleOrFail),
	CompiledReused(OwnModuleOrFail),
}
