use std::borrow::Borrow;

use serde::Serialize;
use serde_json::to_string as to_json_string;

use util::arena::Arena;
use util::dict::{Dict, MutDict};
use util::file_utils::{read_files_in_directory_recursive_if_exists, write_file,
                       write_file_and_ensure_directory, IoError};
use util::iter::KnownLen;
use util::list::List;
use util::loc::LineAndColumnGetter;
use util::path::Path;
use util::string_maker::{Show, Shower, WriteShower};

use compiler::{compile, CompileResult, CompiledProgram, EXTENSION};
use model::diag::{show_diagnostics, Diagnostic};
use model::module::{Module, ModuleOrFail};
use parse::parse;

mod show_equals;
mod test_document_provider;
use self::show_equals::show_equals;
use self::test_document_provider::{ExpectedDiagnostic, TestDocumentProvider};

lazy_static! {
	static ref CASES_ROOT_DIR: Path<'static> = Path::of_slice(b"tests/cases");
	static ref BASELINES_ROOT_DIR: Path<'static> = Path::of_slice(b"tests/cases");
}

enum TestFailure<'a> {
	NoIndex(Path<'a>),
	IoError(IoError),
	ExtraBaselines(&'a [Path<'a>]),
	ExpectedDiagnostics(Dict<Path<'a>, &'a [ExpectedDiagnostic<'a>]>),
	UnexpectedDiagnostics(ModuleOrFail<'a>),
	NoSuchBaseline(Path<'a>),
	UnexpectedOutput { actual: &'a [u8], expected: &'a [u8] },
}
impl<'t, 'a> Show for &'t TestFailure<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			TestFailure::NoIndex(path) => {
				s.add("Could not find an index file at ")?.add(path)?;
				Ok(())
			}
			TestFailure::IoError(_) => unimplemented!(),
			TestFailure::ExtraBaselines(_) => unimplemented!(),
			TestFailure::ExpectedDiagnostics(_) => unimplemented!(),
			TestFailure::UnexpectedDiagnostics(ref m) => show_diagnostics(m, s),
			TestFailure::NoSuchBaseline(_) => unimplemented!(),
			TestFailure::UnexpectedOutput { .. } => unimplemented!(),
		}
	}
}

type TestResult<'a, T> = ::std::result::Result<T, TestFailure<'a>>;

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
	//let test_data = run_compiler_test(Path::from_parts(arr![test_name]));
	//run_test(test-
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
			return Err(TestFailure::UnexpectedDiagnostics(*builtin_module))
		}
	}

	match root {
		ModuleOrFail::Module(m) =>
			if any_diagnostics(m) {
				test_with_diagnostics(
					baselines_directory,
					&program,
					&mut expected_baselines,
					&mut expected_diagnostics,
					update_baselines,
					arena,
				)
			} else {
				if !expected_diagnostics.is_empty() {
					return Err(TestFailure::ExpectedDiagnostics(expected_diagnostics.freeze()))
				}
				test_with_no_diagnostics(
					test_path,
					baselines_directory,
					&program,
					m,
					&mut expected_baselines,
					update_baselines,
					arena,
				)
			},
		ModuleOrFail::Fail(_) =>
			test_with_diagnostics(
				baselines_directory,
				&program,
				&mut expected_baselines,
				&mut expected_diagnostics,
				update_baselines,
				arena,
			),
	}?;

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

fn test_with_diagnostics<'a>(
	baselines_directory: Path,
	program: &CompiledProgram<'a>,
	expected_baselines: &mut MutDict<Path, &'a [u8]>,
	expected_diagnostics_by_full_path: &mut MutDict<Path, &[ExpectedDiagnostic]>,
	update_baselines: bool,
	arena: &'a Arena,
) -> TestResult<'a, ()> {
	for &module_or_fail in program.modules.values() {
		let source = module_or_fail.source().assert_normal();
		let module_path_without_extension = source.full_path.without_extension(EXTENSION);

		let text = &source.document.text;

		if let ModuleOrFail::Module(module) = module_or_fail {
			// If it succeeded, parsing must succeed too.
			// Normally the AST is dropped after parsing, so parse it again for the basline.
			//pub fn parse<'ast, 'text: 'ast>(arena: &'ast Arena, source: &'text [u8]) -> Result<ast::Module<'ast>> {
			let parse_arena = Arena::new();
			let ast = parse(&parse_arena, text).unwrap_or_else(|_| unreachable!());
			assert_baseline(
				baselines_directory,
				module_path_without_extension,
				EXT_AST,
				&ast,
				expected_baselines,
				update_baselines,
				arena,
			)?;

			assert_baseline(
				baselines_directory,
				module_path_without_extension,
				EXT_MODEL,
				&module.class,
				expected_baselines,
				update_baselines,
				arena,
			)?;

			//TODO: bytecode baseline
		}

		let actual_diagnostics = module_or_fail.diagnostics();
		let diagnostics_ok = match expected_diagnostics_by_full_path.get(&source.full_path) {
			Some(expected_diagnostics) => diagnostics_match(text, actual_diagnostics, expected_diagnostics),
			None => !actual_diagnostics.is_empty(),
		};
		if !diagnostics_ok {
			//TODO: show a diff if there were expected diagnostics?
			return Err(TestFailure::UnexpectedDiagnostics(module_or_fail))
		}
	}

	Ok(())
}

fn test_with_no_diagnostics<'a>(
	test_path: Path,
	baselines_directory: Path,
	program: &CompiledProgram,
	index_module: &Module,
	expected_baselines: &mut MutDict<Path, &'a [u8]>,
	update_baselines: bool,
	arena: &'a Arena,
) -> TestResult<'a, ()> {
	unused!(
		test_path,
		baselines_directory,
		program,
		index_module,
		expected_baselines,
		update_baselines,
		arena
	);
	unimplemented!()
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

fn assert_baseline<'a, T: Serialize>(
	test_directory: Path,
	module_path_without_extension: Path,
	extension: &[u8],
	actual: &T,
	baselines: &mut MutDict<Path, &'a [u8]>,
	update_baselines: bool,
	arena: &'a Arena,
) -> TestResult<'a, ()> {
	assert_baseline_worker(
		test_directory,
		module_path_without_extension,
		extension,
		to_json(actual, arena),
		baselines,
		update_baselines,
		arena,
	)
}

fn to_json<'out, T: Serialize>(value: &T, arena: &'out Arena) -> &'out [u8] {
	//TODO:PERF (also sanity)
	//to_json_string(actual).unwrap().into_boxed_str().as_bytes(),
	arena.copy_slice(to_json_string(value).unwrap().as_bytes())
}

fn assert_baseline_worker<'a>(
	test_directory: Path,
	module_path_without_extension: Path,
	extension: &[u8],
	actual: &'a [u8],
	baselines: &mut MutDict<Path, &'a [u8]>,
	update_baselines: bool,
	arena: &'a Arena,
) -> TestResult<'a, ()> {
	let baseline_path = module_path_without_extension.add_extension(extension, arena);
	let full_baseline_path = Path::resolve_with_root(test_directory, baseline_path, arena);

	match baselines.try_extract::<[u8]>(baseline_path.borrow()) {
		Some(expected) =>
			if actual == expected {
				Ok(())
			} else if update_baselines {
				io_result_to_result(write_file(full_baseline_path, actual))
			} else {
				Err(TestFailure::UnexpectedOutput { actual, expected })
			},
		None => {
			// This baseline didn't exist before.
			if update_baselines {
				io_result_to_result(write_file_and_ensure_directory(full_baseline_path, actual))
			} else {
				Err(TestFailure::NoSuchBaseline(full_baseline_path))
			}
		}
	}
}

fn io_result_to_result<'a, T>(io_result: Result<T, IoError>) -> Result<T, TestFailure<'a>> {
	io_result.map_err(TestFailure::IoError)
}

fn any_diagnostics(module: &Module) -> bool {
	!module.diagnostics.is_empty() || module.imports.iter().any(|m| any_diagnostics(m))
}
