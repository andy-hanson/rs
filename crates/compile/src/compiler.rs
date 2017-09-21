use util::arena::{Arena, NoDrop};
use util::dict::MutDict;
use util::iter::KnownLen;
use util::late::Late;
use util::list::{List, ListBuilder};
use util::path::{Path, RelPath};
use util::sym::Sym;
use util::up::Up;

use ast::{Class as ClassAst, Import as ImportAst, Module as ModuleAst};

use check::check_module;

use host::document_provider::DocumentProvider;

use parse::parse;

use model::builtins::BuiltinsOwn;
use model::diag::{Diag, Diagnostic};
use model::document_info::DocumentInfo;
use model::module::{FailModule, Module, ModuleOrFail, ModuleSource, ModuleSourceEnum};
use model::program::CompiledProgram;

use super::CompileResult;
use super::builtins::get_builtins;
use super::module_resolver::{get_document_from_logical_path, GetDocumentResult};

pub fn compile<'a, 'old, D: DocumentProvider<'a>>(
	path: Path<'a>,
	document_provider: &mut D,
	old_program: Option<CompiledProgram<'old>>,
	arena: &'a Arena,
) -> Result<CompileResult<'a>, D::Error> {
	let (builtins, old_modules) = match old_program {
		Some(CompiledProgram { builtins, modules, root }) => {
			unused!(builtins, modules, root);
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
			CompileResult::RootFound(CompiledProgram { builtins, modules, root })
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
	document_provider: &'document_provider mut D,
	// We consume the old program, so we module values out of its map when we reuse them.
	// Intentionally using `Path<'model>` instead of `Path<'old>` as keys,
	// so we can look this up using Path<'model> keys.
	old_modules: MutDict<Path<'model>, ModuleOrFail<'old>>,
	// Keys are logical paths.
	modules: MutDict<Path<'model>, ModuleState<'model>>,
}
impl<'document_provider, 'old, 'model, D: DocumentProvider<'model>>
	Compiler<'document_provider, 'old, 'model, D> {
	fn compile_single(
		&mut self,
		logical_path: Path<'model>,
	) -> Result<(CompileSingleResult<'model>, bool), D::Error> {
		if let Some(already_compiled) = self.modules.get(logical_path) {
			return Ok(match *already_compiled {
				ModuleState::Compiling =>
					// TODO: attach an error to the calling module
					(CompileSingleResult::Circular, false),
				ModuleState::CompiledFresh(module_or_fail) =>
					(CompileSingleResult::Found(module_or_fail), false),
				ModuleState::CompiledReused(module_or_fail) =>
					// Already compiled in the new program.
					// This can happen if the same module is a dependency of two other modules.
					(CompileSingleResult::Found(module_or_fail), true),
			})
		}

		match get_document_from_logical_path(self.document_provider, logical_path, self.arena)? {
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
			Ok(ModuleAst { imports, class }) => {
				self.modules.add(logical_path) <- ModuleState::Compiling;
				let (module_or_fail, is_reused) =
					self.do_compile_single(logical_path, document, imports, class, full_path, is_index)?;
				let module_state = if is_reused {
					ModuleState::CompiledReused(module_or_fail)
				} else {
					ModuleState::CompiledFresh(module_or_fail)
				};
				self.modules.change(&logical_path) <- module_state;
				(CompileSingleResult::Found(module_or_fail), is_reused)
			}
			Err(parse_diag) => {
				let source =
					ModuleSourceEnum::Normal(ModuleSource { logical_path, full_path, is_index, document });
				//TODO: duplicate code somewhere
				let diag = Diagnostic { loc: parse_diag.0, diag: Diag::ParseError(parse_diag.1) };
				let fail = self.arena <- FailModule { source, imports: &[], diagnostics: List::single(diag, self.arena) };
				let module_or_fail = ModuleOrFail::Fail(fail);
				self.modules.add(logical_path) <- ModuleState::CompiledFresh(module_or_fail);
				(CompileSingleResult::Found(ModuleOrFail::Fail(fail)), false)
			}
		})
	}

	fn do_compile_single<'ast>(
		&mut self,
		logical_path: Path<'model>,
		document: DocumentInfo<'model>,
		import_asts: List<'ast, ImportAst<'ast>>,
		class_ast: &'ast ClassAst<'ast>,
		full_path: Path<'model>,
		is_index: bool,
	) -> Result<(ModuleOrFail<'model>, bool), D::Error> {
		let (resolved_imports, all_dependencies_reused) = self.resolve_imports(full_path, import_asts)?;

		// We will only bother looking at the old module if all of our dependencies were safely reused.
  // If oldModule doesn't exactly match, we'll ignore it completely.
		if all_dependencies_reused {
			if let Some(old_module_or_fail) = self.old_modules.try_extract(logical_path) {
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

		let source = ModuleSourceEnum::Normal(ModuleSource { logical_path, full_path, is_index, document });
		let res = match resolved_imports {
			ResolvedImports::Success(imports) => {
				let module = self.arena <- Module { source, imports, class: Late::new(), diagnostics: Late::new() };
				let name = match logical_path.file_name() {
					Some(name) => Sym::of(name),
					None => self.document_provider.root_name(),
				};
				// Initializes module.class and module.diagnostics.
	// (There are no parse/import diagnostics or we wouldn't have gotten here.)
				check_module(module, self.builtins, class_ast, name, self.arena);
				ModuleOrFail::Module(module)
			}
			ResolvedImports::Failure(imports, diagnostics) =>
				ModuleOrFail::Fail(self.arena <- FailModule { source, imports, diagnostics }),
		};
		Ok((res, false))
	}

	fn resolve_imports<'ast>(
		&mut self,
		full_path: Path<'model>,
		import_asts: List<'ast, ImportAst<'ast>>,
	) -> Result<(ResolvedImports<'model>, bool), D::Error> {
		let mut diagnostics = ListBuilder::<Diagnostic>::new(self.arena);
		let mut all_imports_reused = true;
		//TODO:PERF probably, all imports will succeed, so allocate whole array.
		let mut all_successes = self.arena
			.max_len_builder::<Up<'model, Module<'model>>>(import_asts.len());
		//TODO:PERF probably, this will not be necessary, somehow avoid allocating?
		let mut all = self.arena
			.exact_len_builder::<ModuleOrFail<'model>>(import_asts.len());
		let mut any_failure = false;
		for import_ast in import_asts {
			match self.resolve_import(import_ast, &mut diagnostics, full_path, &mut all_imports_reused)? {
				Some(import) => {
					if let ModuleOrFail::Module(module) = import {
						&mut all_successes <- Up(module);
					} else {
						any_failure = true
					}
					&mut all <- import;
				}
				None => any_failure = true,
			}
		}

		let diags = diagnostics.finish();
		let resolved = if any_failure {
			ResolvedImports::Failure(all.finish(), diags)
		} else {
			assert!(diags.is_empty());
			ResolvedImports::Success(all_successes.finish())
		};
		Ok((resolved, all_imports_reused))
	}

	fn resolve_import<'ast>(
		&mut self,
		import_ast: &'ast ImportAst<'ast>,
		import_diagnostics: &mut ListBuilder<'model, Diagnostic<'model>>,
		full_path: Path<'model>,
		all_dependencies_reused: &mut bool,
	) -> Result<Option<ModuleOrFail<'model>>, D::Error> {
		match *import_ast {
			ImportAst::Global(_loc, _path) => {
				unimplemented!() // Get the builtin with this name
			}
			ImportAst::Local(loc, rel_path) => {
				let relative_path = RelPath::clone_path_to_arena(rel_path, self.arena);
				let (imported_module, is_import_reused) =
					self.compile_single(full_path.resolve(relative_path, self.arena))?;
				*all_dependencies_reused &= is_import_reused;
				Ok(match imported_module {
					CompileSingleResult::Circular => {
						import_diagnostics <-
							Diagnostic { loc, diag: Diag::CircularDependency { from: full_path, to: relative_path } };
						None
					}
					CompileSingleResult::Missing => {
						import_diagnostics <-
							Diagnostic { loc, diag: Diag::CantFindLocalModule { from: full_path, to: relative_path } };
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
impl<'model> NoDrop for ModuleState<'model> {}
