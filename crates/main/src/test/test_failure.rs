use util::file_utils::IoError;
use util::path::Path;
use util::string_maker::{Show, Shower};

use model::diag::show_diagnostics;
use model::module::ModuleOrFail;

use interpret::emit::emit_error::EmitError;

use super::test_document_provider::ExpectedDiagnostic;

pub type TestResult<'a, T> = ::std::result::Result<T, TestFailure<'a>>;

pub enum TestFailure<'a> {
	NoIndex(Path<'a>),
	IoError(IoError),
	ExtraBaselines(&'a [Path<'a>]),
	DiagnosticsMismatch { module_or_fail: ModuleOrFail<'a>, expected: &'a [ExpectedDiagnostic<'a>] },
	NoSuchBaseline(Path<'a>),
	UnexpectedOutput { actual: &'a [u8], expected: &'a [u8] },
	EmitError(EmitError<'a>),
}
impl<'t, 'a> Show for &'t TestFailure<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			TestFailure::NoIndex(path) => {
				s.add("Could not find an index file at ")?.add(path)?;
			}
			TestFailure::IoError(_) => unimplemented!(),
			TestFailure::ExtraBaselines(_) => unimplemented!(),
			TestFailure::DiagnosticsMismatch { module_or_fail, expected } => {
				unused!(expected); //TODO: show a diff
				s.add("Unexpected diagnostics:\n")?;
				show_diagnostics(module_or_fail, s)?;
			}
			TestFailure::NoSuchBaseline(path) => {
				s.add("Baseline ")?.add(path)?.add(" does not yet exist.")?;
			},
			TestFailure::UnexpectedOutput { .. } => unimplemented!(),
			TestFailure::EmitError(ref e) => {
				s.add(e)?;
			},
		}
		Ok(())
	}
}

pub fn io_result_to_result<'a, T>(io_result: Result<T, IoError>) -> Result<T, TestFailure<'a>> {
	io_result.map_err(TestFailure::IoError)
}
