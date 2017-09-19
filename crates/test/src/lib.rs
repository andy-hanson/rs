#![feature(placement_in_syntax)]

extern crate diff;
extern crate serde;
extern crate serde_yaml;

extern crate compile;
extern crate host;
extern crate interpret;
extern crate model;
extern crate parse;
#[macro_use]
extern crate util;

use serde::{Serialize, Serializer};
use serde::ser::SerializeMap;

use util::arena::Arena;
use util::dict::MutDict;
use util::file_utils::{ReadFileOptions, read_files_in_directory_recursive_if_exists};
use util::iter::KnownLen;
use util::list::List;
use util::loc::LineAndColumnGetter;
use util::path::Path;
use util::output_shower::OutputShower;
use util::sym::Sym;
use util::up::Up;

use compile::{compile, CompileResult};
use model::class::ClassDeclaration;
use model::diag::Diagnostic;
use model::module::ModuleOrFail;
use model::program::CompiledProgram;
use interpret::emitted_model::{Code, EmittedProgram};
use interpret::run::run_method;
use interpret::emit::emit_program;
use parse::parse;

mod baselines;
mod show_equals;
mod test_document_provider;
mod test_failure;
use self::baselines::Baselines;
use self::show_equals::show_equals;
use self::test_document_provider::{ExpectedDiagnostic, TestDocumentProvider};
use self::test_failure::{TestFailure, TestResult};

pub use self::baselines::BaselinesUpdate;

const CASES_ROOT_DIR: &'static [u8] = b"tests/cases";
const BASELINES_ROOT_DIR: &'static [u8] = b"tests/baselines";

pub fn do_test_single(test_path: Path, update_baselines: BaselinesUpdate) -> i32 {
	let arena = Arena::new();
	match test_single(test_path, update_baselines, &arena) {
		Ok(()) => 0,
		Err(e) => {
			OutputShower::write_stderr(&e).unwrap();
			1
		}
	}
}

fn test_single<'a>(test_path: Path, update_baselines: BaselinesUpdate, arena: &'a Arena) -> TestResult<'a, ()> {
	let test_directory = Path::resolve_with_root(Path::of(CASES_ROOT_DIR), test_path, arena);
	let baselines_directory = Path::resolve_with_root(Path::of(BASELINES_ROOT_DIR), test_path, arena);

	let mut document_provider = TestDocumentProvider::new(test_directory);
	let program = {
		let compile_result = compile(Path::EMPTY, &mut document_provider, None, arena)?;
		match compile_result {
			CompileResult::RootMissing => return Err(TestFailure::NoIndex(document_provider.into_root_dir())),
			CompileResult::RootFound(program) => program,
		}
	};
	let mut expected_diagnostics = document_provider.get_expected_diagnostics();
	let mut expected_baselines =
		read_files_in_directory_recursive_if_exists(baselines_directory, ReadFileOptions::Plain, arena)?;

	for builtin_module in *program.builtins.all {
		if !builtin_module.diagnostics().is_empty() {
			return Err(TestFailure::DiagnosticsMismatch { module_or_fail: *builtin_module, expected: &[] })
		}
	}

	{
		let mut baselines =
			Baselines { arena, baselines_directory, update_baselines, expected: &mut expected_baselines };

		test_with_diagnostics(&mut baselines, &program, &mut expected_diagnostics)?;
	}

	if expected_baselines.is_empty() {
		Ok(())
	} else {
		Err(TestFailure::ExtraBaselinesOnDisk(arena.map(&expected_baselines, |(path, _)| *path)))
	}
}

const EXT_AST: &[u8] = b".ast";
const EXT_MODEL: &[u8] = b".model";
const EXT_EMIT: &[u8] = b".emit";

fn test_with_diagnostics<'model, 'expected>(
	baselines: &mut Baselines<'model, 'expected>,
	program: &CompiledProgram<'model>,
	expected_diagnostics_by_path: &mut MutDict<Path, &'model [ExpectedDiagnostic]>,
) -> TestResult<'model, ()> {
	let mut any_diagnostics = false;
	for &module_or_fail in program.modules.values() {
		let source = module_or_fail.source().assert_normal();
		let text = &source.document.text;

		// Normally the AST is dropped after type-checking, so parse it again.
		let parse_arena = Arena::new();
		// If parsing fails, just don't make a baseline -- rely on diagnostics.
		if let Ok(ast) = parse(&parse_arena, text) {
			baselines.assert_baseline(module_or_fail, EXT_AST, &ast)?;
		}

		let actual_diagnostics = module_or_fail.diagnostics();
		any_diagnostics = any_diagnostics || !actual_diagnostics.is_empty();
		let expected_diagnostics = expected_diagnostics_by_path
			.get(source.full_path)
			.cloned()
			.unwrap_or(&[]);
		if !diagnostics_match(text, actual_diagnostics, expected_diagnostics) {
			return Err(TestFailure::DiagnosticsMismatch { module_or_fail, expected: expected_diagnostics })
		}

		if let ModuleOrFail::Module(module) = module_or_fail {
			baselines.assert_baseline(module_or_fail, EXT_MODEL, &module.class)?;
			//TODO: bytecode baseline
		}
	}

	if !any_diagnostics {
		test_interpret(baselines, program)?
	}

	Ok(())
}

fn test_interpret<'model, 'expected>(
	baselines: &mut Baselines<'model, 'expected>,
	program: &CompiledProgram<'model>,
) -> TestResult<'model, ()> {
	let emit_arena = Arena::new();
	let emitted_program = match emit_program(program, &emit_arena) {
		Ok(p) => p,
		Err(e) => return Err(TestFailure::EmitError(e)),
	};

	for &module_or_fail in program.modules.values() {
		let module = match module_or_fail { ModuleOrFail::Module(m) => m, _ => unreachable!(), };
		baselines.assert_baseline(ModuleOrFail::Module(module), EXT_EMIT, &ModuleEmitted { emitted_program: &emitted_program, class: &*module.class })?
	}

	let main = program.root.assert_success().class.find_static_method(Sym::of(b"main")).unwrap(); //TODO: diagnostic if "main" not found
	run_method(program, Up(main), &emitted_program);
	Ok(())
}

struct ModuleEmitted<'model : 'emit, 'emit> {
	emitted_program: &'emit EmittedProgram<'model, 'emit>,
	class: &'model ClassDeclaration<'model>,
}
impl<'model, 'emit> Serialize for ModuleEmitted<'model, 'emit> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut map = serializer.serialize_map(None)?;
		for an_impl in self.class.all_impls() {
			match *self.emitted_program.methods.get_impl(Up(an_impl)) {
				Code::Builtin(_) => unreachable!(),
				Code::Instructions(ref i) => {
					map.serialize_entry(&an_impl.name(), i)?
				}
			}
		}
		for method in *self.class.methods {
			//TODO:duplicate code of above
			match *self.emitted_program.methods.get_method(Up(method)) {
				Code::Builtin(_) => unreachable!(),
				Code::Instructions(ref i) => {
					map.serialize_entry(&method.name(), i)?
				}
			}
		}
		map.end()
	}
}



fn diagnostics_match(text: &[u8], actual: List<Diagnostic>, expected: &[ExpectedDiagnostic]) -> bool {
	let lc = LineAndColumnGetter::new(text);
	if actual.len() != expected.len() {
		return false
	}

	//TODO:PERF
	let arena = Arena::new();
	let actual_sorted = arena.map(actual, |x| x);
	actual_sorted.sort_unstable_by_key(|x| x.loc.start.index);

	actual_sorted.each_corresponds(
		expected,
		|&&Diagnostic { loc, ref diag }, &ExpectedDiagnostic(lc_loc, diag_text)| {
			lc.loc_at_line_and_column(lc_loc) == loc && show_equals(diag, diag_text)
		},
	)
}
