use std::borrow::Borrow;

use util::arena::{Arena, List, ListBuilder, Up};
use util::dict::MutDict;
use util::late::Late;
use util::path::{Path, RelPath};
use util::sym::Sym;

use host::document_provider::DocumentProvider;

use model::diag::{Diag, Diagnostic};
use model::document_info::DocumentInfo;
use model::module::{FailModule, Module, ModuleOrFail, ModuleSource, ModuleSourceEnum};

use super::super::builtins::{get_builtins, BuiltinsOwn};
use super::super::check::check_module;
use super::super::parse::ast::{Class as ClassAst, Import as ImportAst, Module as ModuleAst};
use super::super::parse::parse;

use super::{CompileResult, CompiledProgram};
use super::module_resolver::{get_document_from_logical_path, GetDocumentResult};

pub fn compile<'a, 'old, D: DocumentProvider<'a>>(
	path: Path<'a>,
	document_provider: &D,
	old_program: Option<CompiledProgram<'old>>,
	arena: &'a Arena,
) -> Result<CompileResult<'a>, D::Error> {
	let (builtins, old_modules) = match old_program {
		Some(CompiledProgram { builtins, modules }) => {
			unused!(builtins, modules);
			unimplemented!()
		}
		None =>
			// Use an empty MutDict as the old modules
			(get_builtins(arena), MutDict::new()),
	};
	// Don't care about isReused for top level
	let (result, modules_states) = {
		let mut compiler =
			Compiler { arena, builtins, document_provider, old_modules, modules: MutDict::new() };
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
			CompileResult::RootFound {
				program: new_program,
				root,
			}
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
struct Compiler<'document_provider, 'old, 'model, D: DocumentProvider<'model> + 'document_provider> {
	arena: &'model Arena,
	builtins: &'model BuiltinsOwn<'model>,
	document_provider: &'document_provider D,
	// We consume the old program, so we module values out of its map when we reuse them.
	old_modules: MutDict<Path<'old>, ModuleOrFail<'old>>,
	// Keys are logical paths.
	modules: MutDict<Path<'model>, ModuleState<'model>>,
}
impl<'document_provider, 'old, 'model, D: DocumentProvider<'model>>
	Compiler<'document_provider, 'old, 'model, D> {
	fn compile_single(
		&mut self,
		logical_path: Path<'model>,
	) -> Result<(CompileSingleResult<'model>, bool), D::Error> {
		if let Some(already_compiled) = self.modules.get(&logical_path) {
			return Ok(match *already_compiled {
				ModuleState::Compiling =>
					// TODO: attach an error to the calling module
					(CompileSingleResult::Circular, false),
				ModuleState::CompiledFresh(ref module_or_fail) =>
					(CompileSingleResult::Found(module_or_fail.clone_as_ptr()), false),
				ModuleState::CompiledReused(ref module_or_fail) =>
					// Already compiled in the new program.
					// This can happen if the same module is a dependency of two other modules.
					(CompileSingleResult::Found(module_or_fail.clone_as_ptr()), true),
			})
		}

		match get_document_from_logical_path(self.document_provider, &logical_path, self.arena)? {
			GetDocumentResult::Found { full_path, is_index, document } =>
				self.compile_module_from_document(logical_path, full_path, is_index, document),
			GetDocumentResult::NotFound => Ok((CompileSingleResult::Missing, false)),
		}
	}

	fn compile_module_from_document(
		&mut self,
		borrow_logical_path: Path,
		full_path: Path<'model>,
		is_index: bool,
		document: DocumentInfo<'model>,
	) -> Result<(CompileSingleResult<'model>, bool), D::Error> {
		let logical_path = borrow_logical_path.clone_path_to_arena(self.arena);
		let parse_arena = Arena::new();
		let parse_result = parse(&parse_arena, document.text);
		Ok(match parse_result {
			Ok(ModuleAst { ref imports, class }) => {
				self.modules
					.add(logical_path.clone_path_as_ptr(), ModuleState::Compiling);
				let (module_or_fail, is_reused) =
					self.do_compile_single(&logical_path, document, imports, class, full_path, is_index)?;
				let module_state = if is_reused {
					ModuleState::CompiledReused(module_or_fail.clone_as_ptr())
				} else {
					ModuleState::CompiledFresh(module_or_fail.clone_as_ptr())
				};
				self.modules.change(&logical_path, module_state);
				(CompileSingleResult::Found(module_or_fail), is_reused)
			}
			Err(parse_diag) => {
				let source = ModuleSourceEnum::Normal(
					ModuleSource { logical_path: logical_path.clone_path_as_ptr(), is_index, document },
				);
				let diag = Diagnostic(parse_diag.0, Diag::ParseError(parse_diag.1)); //TODO: duplicate code somewhere
				let fail = self.arena <- FailModule { source, imports: &[], diagnostics: List::single(diag, self.arena) };
				let module_or_fail = ModuleOrFail::Fail(fail);
				self.modules
					.add(logical_path.clone_path_as_ptr(), ModuleState::CompiledFresh(module_or_fail));
				(CompileSingleResult::Found(ModuleOrFail::Fail(fail)), false)
			}
		})
	}

	fn do_compile_single<'ast>(
		&mut self,
		logical_path: &Path<'model>,
		document: DocumentInfo<'model>,
		import_asts: &'ast List<'ast, ImportAst<'ast>>,
		class_ast: &'ast ClassAst<'ast>,
		full_path: Path<'model>,
		is_index: bool,
	) -> Result<(ModuleOrFail<'model>, bool), D::Error> {
		let (resolved_imports, all_dependencies_reused) = self.resolve_imports(&full_path, import_asts)?;

		// We will only bother looking at the old module if all of our dependencies were safely reused.
		// If oldModule doesn't exactly match, we'll ignore it completely.
		if all_dependencies_reused {
			if let Some(old_module_or_fail) = self.old_modules.try_extract::<[u8]>(logical_path.borrow()) {
				// old_modules only stores modules from source code, not builtins,
				// so unwrap() should succeed.
				if old_module_or_fail
					.source()
					.assert_normal()
					.document
					.same_version_as(&document)
				{
					return Ok((old_module_or_fail.copy_to_new_arena(self.arena), true))
				}
			}
		}

		let source = ModuleSourceEnum::Normal(
			ModuleSource { logical_path: logical_path.clone_path_as_ptr(), is_index, document },
		);
		let res = match resolved_imports {
			ResolvedImports::Success(imports) => {
				let module =
					self.arena <- Module { source, imports, class: Late::new(), diagnostics: Late::new() };
				let name = match logical_path.last() {
					Some(name) => Sym::from_slice(name),
					None => self.document_provider.root_name(),
				};
				// Initializes module.class and module.diagnostics.
				// (There are no parse/import diagnostics or we wouldn't have gotten here.)
				check_module(module, &self.builtins.as_ctx(), class_ast, name, self.arena);
				ModuleOrFail::Module(module)
			}
			ResolvedImports::Failure(imports, diagnostics) =>
				ModuleOrFail::Fail(self.arena <- FailModule { source, imports, diagnostics }),
		};
		Ok((res, false))
	}

	fn resolve_imports<'ast>(
		&mut self,
		full_path: &Path<'model>,
		import_asts: &'ast List<'ast, ImportAst<'ast>>,
	) -> Result<(ResolvedImports<'model>, bool), D::Error> {
		let mut diagnostics = self.arena.list_builder::<Diagnostic>();
		let mut all_imports_reused = true;
		//TODO:PERF probably, all imports will succeed, so allocate whole array.
		let all_successes = self.arena
			.max_size_arr_builder::<Up<'model, Module<'model>>>(import_asts.len);
		//TODO:PERF probably, this will not be necessary, somehow avoid allocating?
		let all = self.arena
			.max_size_arr_builder::<ModuleOrFail<'model>>(import_asts.len);
		let mut any_failure = false;
		for import_ast in import_asts.iter() {
			match self.resolve_import(import_ast, &mut diagnostics, full_path, &mut all_imports_reused)? {
				Some(import) => {
					if let ModuleOrFail::Module(module) = import {
						&all_successes <- Up(module);
					} else {
						any_failure = true
					}
					&all <- import;
				}
				None => any_failure = true,
			}
		}

		let diags = diagnostics.finish();
		let resolved = if any_failure {
			ResolvedImports::Failure(all.finish(), diags)
		} else {
			assert_eq!(diags.len, 0);
			ResolvedImports::Success(all_successes.finish())
		};
		Ok((resolved, all_imports_reused))
	}

	fn resolve_import<'ast>(
		&mut self,
		import_ast: &'ast ImportAst<'ast>,
		import_diagnostics: &mut ListBuilder<'model, Diagnostic<'model>>,
		full_path: &Path<'model>,
		all_dependencies_reused: &mut bool,
	) -> Result<Option<ModuleOrFail<'model>>, D::Error> {
		match *import_ast {
			ImportAst::Global(_loc, ref _path) => {
				unimplemented!() // Get the builtin with this name
			}
			ImportAst::Local(loc, ref rel_path) => {
				let relative_path = RelPath::clone_path_to_arena(rel_path, self.arena);
				let (imported_module, is_import_reused) =
					self.compile_single(full_path.resolve(&relative_path))?;
				*all_dependencies_reused &= is_import_reused;
				Ok(match imported_module {
					CompileSingleResult::Circular => {
						import_diagnostics.add() <-
							Diagnostic(loc, Diag::CircularDependency { from: full_path.clone_path_as_ptr(), to: relative_path });
						None
					}
					CompileSingleResult::Missing => {
						import_diagnostics.add() <-
							Diagnostic(loc, Diag::CantFindLocalModule { from: full_path.clone_path_as_ptr(), to: relative_path });
						None
					}
					CompileSingleResult::Found(found) => Some(found),
				})
			}
		}
	}
}

enum ResolvedImports<'model> {
	Success(&'model [Up<'model, Module<'model>>]),
	Failure(&'model [ModuleOrFail<'model>], List<'model, Diagnostic<'model>>),
}

enum CompileSingleResult<'model> {
	Missing,
	Circular,
	Found(ModuleOrFail<'model>),
}

// Most modules will not be `Compiling` at any given time,
// so OK that the difference in size between Compiling and the others is large.
#[allow(large_enum_variant)]
enum ModuleState<'model> {
	Compiling,
	CompiledFresh(ModuleOrFail<'model>),
	CompiledReused(ModuleOrFail<'model>),
}
