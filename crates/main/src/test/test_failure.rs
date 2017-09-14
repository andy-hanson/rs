use util::file_utils::IoError;
use util::path::Path;
use util::string_maker::{Show, Shower};

use model::diag::show_diagnostics;
use model::module::ModuleOrFail;

use super::test_document_provider::ExpectedDiagnostic;

pub type TestResult<'a, T> = ::std::result::Result<T, TestFailure<'a>>;

pub enum TestFailure<'a> {
	NoIndex(Path<'a>),
	IoError(IoError),
	ExtraBaselines(&'a [Path<'a>]),
	DiagnosticsMismatch { module_or_fail: ModuleOrFail<'a>, expected: &'a [ExpectedDiagnostic<'a>] },
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
			TestFailure::DiagnosticsMismatch { module_or_fail, expected } => {
				unused!(module_or_fail, expected);
				show_diagnostics(module_or_fail, s)?;
				unimplemented!()
			}
			TestFailure::NoSuchBaseline(_) => unimplemented!(),
			TestFailure::UnexpectedOutput { .. } => unimplemented!(),
		}
	}
}

pub fn io_result_to_result<'a, T>(io_result: Result<T, IoError>) -> Result<T, TestFailure<'a>> {
	io_result.map_err(TestFailure::IoError)
}
