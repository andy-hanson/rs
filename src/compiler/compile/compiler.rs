use util::arr::{Arr, ArrBuilder};
use util::dict::MutDict;
use util::path::Path;
use util::ptr::{LateOwn, Own, Ptr};
use util::sym::Sym;

use super::super::super::host::document_info::DocumentInfo;
use super::super::super::host::document_provider::DocumentProvider;
use super::super::super::host::file_input::Result as IoResult;
use super::super::super::model::diag::{Diag, Diagnostic};
use super::super::super::model::module::{FailModule, Module, ModuleSource, OwnModuleOrFail, PtrModuleOrFail};

use super::super::builtins::{get_builtins, BuiltinsOwn};
use super::super::check::check_module;
use super::super::parse::ast::{Class as ClassAst, Import as ImportAst, Module as ModuleAst};
use super::super::parse::parse;

use super::{CompileResult, CompiledProgram};
use super::module_resolver::{get_document_from_logical_path, GetDocumentResult};

pub fn compile(
	path: Path,
	document_provider: &DocumentProvider,
	old_program: Option<CompiledProgram>,
) -> IoResult<CompileResult> {
	let (builtins, old_modules) = match old_program {
		Some(CompiledProgram { builtins, modules }) => (builtins, modules),
		None =>
			// Use an empty MutDict as the old modules
			(get_builtins(), MutDict::new()),
	};
	// Don't care about isReused for top level
	let (result, modules_states) = {
		let mut compiler =
			Compiler { builtins: &builtins, document_provider, old_modules, modules: MutDict::new() };
		let res = compiler.compile_single(path)?.0;
		(res, compiler.modules)
	};
	Ok(match result {
		CompileSingleResult::Found(root) => {
			let modules = modules_states.map_values(|o| match o {
					ModuleState::CompiledFresh(m) | ModuleState::CompiledReused(m) => m,
					ModuleState::Compiling => unreachable!(),
				});
			let new_program = CompiledProgram { builtins, modules };
			CompileResult::RootFound(new_program, root)
		}
		CompileSingleResult::Missing =>
			CompileResult::RootMissing,
		CompileSingleResult::Circular =>
			// Very first module can't be circular
			unreachable!(),
	})
}

/*
Design notes:

We assume that there may be file system events that we don't know about.
Therefore, we will always ask the CompileHost for the latest versions of all files.
The CompilerHost may implement caching. (In a command-line scenario this should not be necessary.)
Whether a document may be reused is indicated by its version vs the version the compiler used.
*/
struct Compiler<'a> {
	builtins: &'a BuiltinsOwn,
	document_provider: &'a DocumentProvider,
	// We consume the old program, so we module values out of its map when we reuse them.
	old_modules: MutDict<Own<Path>, OwnModuleOrFail>,
	// Keys are logical paths.
	modules: MutDict<Own<Path>, ModuleState>,
}
impl<'a> Compiler<'a> {
	fn compile_single(&mut self, logical_path: Path) -> IoResult<(CompileSingleResult, bool)> {
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
	) -> IoResult<(CompileSingleResult, bool)> {
		let own_logical_path = Own::new(logical_path);
		Ok(match parse(document.source.as_slice()) {
			Ok(ModuleAst { imports, class }) => {
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
				let source = Some(ModuleSource {
					logical_path: own_logical_path.ptr(),
					is_index,
					document_version: document.version,
				});
				let diagnostics = Arr::_1(parse_diag);
				let fail = Own::new(FailModule { source, imports: Arr::empty(), diagnostics });
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
		import_asts: Arr<ImportAst>,
		class_ast: ClassAst,
		full_path: Path,
		is_index: bool,
	) -> IoResult<(OwnModuleOrFail, bool)> {
		let (resolved_imports, all_dependencies_reused) = self.resolve_imports(&full_path, import_asts)?;

		// We will only bother looking at the old module if all of our dependencies were safely reused.
		// If oldModule doesn't exactly match, we'll ignore it completely.
		if all_dependencies_reused {
			if let Some(old_module_or_fail) = self.old_modules.try_extract(&**logical_path) {
				// old_modules only stores modules from source code, not builtins,
				// so unwrap() should succeed.
				if old_module_or_fail.source().unwrap().document_version == document_version {
					return Ok((old_module_or_fail, true))
				}
			}
		}

		let source =
			Some(ModuleSource { logical_path: logical_path.clone_ptr(), is_index, document_version });
		let res = match resolved_imports {
			ResolvedImports::Success(imports) => {
				let module =
					Own::new(Module { source, imports, class: LateOwn::new(), diagnostics: LateOwn::new() });
				let name = match logical_path.last() {
					Some(name) => Sym::from_slice(name.as_slice()),
					None => self.document_provider.root_name(),
				};
				// Initializes module.class and module.diagnostics.
				// (There are no parse/import diagnostics or we wouldn't have gotten here.)
				check_module(&module, &self.builtins.as_ctx(), &class_ast, name);
				OwnModuleOrFail::Module(module)
			}
			ResolvedImports::Failure(imports, diagnostics) => {
				let fail = Own::new(FailModule { source, imports, diagnostics });
				OwnModuleOrFail::Fail(fail)
			}
		};
		Ok((res, false))
	}

	fn resolve_imports(
		&mut self,
		full_path: &Path,
		import_asts: Arr<ImportAst>,
	) -> IoResult<(ResolvedImports, bool)> {
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
		import_ast: ImportAst,
		import_diagnostics: &mut ArrBuilder<Diagnostic>,
		full_path: &Path,
		all_dependencies_reused: &mut bool,
	) -> IoResult<Option<PtrModuleOrFail>> {
		match import_ast {
			ImportAst::Global(_loc, _path) => {
				unimplemented!() // Get the builtin with this name
			}
			ImportAst::Local(loc, relative_path) => {
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

enum CompileSingleResult {
	Missing,
	Circular,
	Found(PtrModuleOrFail),
}

// Most modules will not be `Compiling` at any given time,
// so OK that the difference in size between Compiling and the others is large.
#[allow(large_enum_variant)]
enum ModuleState {
	Compiling,
	CompiledFresh(OwnModuleOrFail),
	CompiledReused(OwnModuleOrFail),
}
