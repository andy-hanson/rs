use util::arena::Arena;
use util::dict::MutDict;
use util::file_utils::read_files_in_directory_recursive_if_exists;
use util::iter::KnownLen;
use util::list::List;
use util::loc::LineAndColumnGetter;
use util::path::Path;
use util::string_maker::WriteShower;

use compiler::{compile, CompileResult, CompiledProgram, EXTENSION};
use model::diag::Diagnostic;
use model::module::{Module, ModuleOrFail};
use parse::parse;

mod baselines;
mod show_equals;
mod test_document_provider;
mod test_failure;
use self::baselines::Baselines;
use self::show_equals::show_equals;
use self::test_document_provider::{ExpectedDiagnostic, TestDocumentProvider};
use self::test_failure::{io_result_to_result, TestFailure, TestResult};

lazy_static! {
	static ref CASES_ROOT_DIR: Path<'static> = Path::of_slice(b"tests/cases");
	static ref BASELINES_ROOT_DIR: Path<'static> = Path::of_slice(b"tests/cases");
}

pub fn do_test_single(test_path: Path, update_baselines: bool) -> i32 {
	let arena = Arena::new();
	match test_single(test_path, update_baselines, &arena) {
		Ok(()) => 0,
		Err(e) => {
			WriteShower::write_stderr(&e).unwrap();
			1
		}
	}
}

fn test_single<'a>(test_path: Path, update_baselines: bool, arena: &'a Arena) -> TestResult<'a, ()> {
	let test_directory = Path::resolve_with_root(*CASES_ROOT_DIR, test_path, arena);
	let baselines_directory = Path::resolve_with_root(*BASELINES_ROOT_DIR, test_path, arena);

	let mut document_provider = TestDocumentProvider::new(test_directory);
	let (program, root) = {
		let compile_result = io_result_to_result(compile(Path::EMPTY, &mut document_provider, None, arena))?;
		match compile_result {
			CompileResult::RootMissing => return Err(TestFailure::NoIndex(document_provider.into_root_dir())),
			CompileResult::RootFound { program, root } => (program, root),
		}
	};
	let mut expected_diagnostics = document_provider.get_expected_diagnostics();
	let mut expected_baselines =
		io_result_to_result(read_files_in_directory_recursive_if_exists(baselines_directory, arena))?;

	for builtin_module in *program.builtins.all {
		if !builtin_module.diagnostics().is_empty() {
			return Err(TestFailure::DiagnosticsMismatch { module_or_fail: *builtin_module, expected: &[] })
		}
	}

	{
		let mut baselines =
			Baselines { arena, baselines_directory, update_baselines, expected: &mut expected_baselines };

		test_with_diagnostics(&mut baselines, &program, root, &mut expected_diagnostics)?;
	}

	let res = if expected_baselines.is_empty() {
		Ok(())
	} else {
		Err(TestFailure::ExtraBaselines(arena.map(&expected_baselines, |(path, _)| *path)))
	};

	//TODO:KILL (and rely on implicit drop order)
	drop(expected_diagnostics);
	drop(program);

	res
}

const EXT_AST: &[u8] = b".ast";
const EXT_MODEL: &[u8] = b".model";

fn test_with_diagnostics<'a, 'expected>(
	baselines: &mut Baselines<'a, 'expected>,
	program: &CompiledProgram<'a>,
	root: ModuleOrFail,
	expected_diagnostics_by_path: &mut MutDict<Path, &'a [ExpectedDiagnostic]>,
) -> TestResult<'a, ()> {
	let mut any_diagnostics = false;
	for &module_or_fail in program.modules.values() {
		let source = module_or_fail.source().assert_normal();
		let module_path_without_extension = source.full_path.without_extension(EXTENSION);

		let text = &source.document.text;

		// Normally the AST is dropped after type-checking, so parse it again.
		let parse_arena = Arena::new();
		// If parsing fails, just don't make a baseline -- rely on diagnostics.
		if let Ok(ast) = parse(&parse_arena, text) {
			baselines.assert_baseline(module_path_without_extension, EXT_AST, &ast)?;
		}

		let actual_diagnostics = module_or_fail.diagnostics();
		any_diagnostics = any_diagnostics || !actual_diagnostics.is_empty();
		let expected_diagnostics = expected_diagnostics_by_path
			.get(&source.full_path)
			.cloned()
			.unwrap_or(&[]);
		if !diagnostics_match(text, actual_diagnostics, expected_diagnostics) {
			return Err(TestFailure::DiagnosticsMismatch { module_or_fail, expected: expected_diagnostics })
		}

		if let ModuleOrFail::Module(module) = module_or_fail {
			baselines.assert_baseline(module_path_without_extension, EXT_MODEL, &module.class)?;
			//TODO: bytecode baseline
		}
	}

	if !any_diagnostics {
		let m = match root {
			ModuleOrFail::Module(m) => m,
			_ => unreachable!(),
		};
		unused!(m);
		//TODO: execute the module and run its assertions
		unimplemented!()
	}

	Ok(())
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

fn any_diagnostics(module: &Module) -> bool {
	!module.diagnostics.is_empty() || module.imports.iter().any(|m| any_diagnostics(m))
}
