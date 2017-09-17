use std::str::from_utf8;

use diff::{lines as diff_lines, Result as Diff};

use util::file_utils::IoError;
use util::path::Path;
use util::show::{Show, Shower};

use model::diag::show_diagnostics;
use model::module::ModuleOrFail;

use interpret::emit::emit_error::EmitError;

use super::test_document_provider::ExpectedDiagnostic;

pub type TestResult<'a, T> = ::std::result::Result<T, TestFailure<'a>>;

pub enum TestFailure<'a> {
	NoIndex(Path<'a>),
	IoError(IoError),
	ExtraBaselinesOnDisk(&'a [Path<'a>]),
	DiagnosticsMismatch { module_or_fail: ModuleOrFail<'a>, expected: &'a [ExpectedDiagnostic<'a>] },
	NoSuchBaseline(Path<'a>),
	BaselineChanged { path: Path<'a>, old: &'a [u8], new: &'a [u8] },
	EmitError(EmitError<'a>),
}
impl<'t, 'a> Show for &'t TestFailure<'a> {
	fn show<S: Shower>(self, s: &mut S) -> Result<(), S::Error> {
		match *self {
			TestFailure::NoIndex(path) => {
				s.add("Could not find an index file at ")?.add(path)?;
			}
			TestFailure::IoError(_) => unimplemented!(),
			TestFailure::ExtraBaselinesOnDisk(baselines) => {
				s.add("The following baselines exist on disk but were not generated: ")?.join_map(baselines, |b| *b)?;
			},
			TestFailure::DiagnosticsMismatch { module_or_fail, expected } => {
				unused!(expected); //TODO: show a diff
				s.add("Unexpected diagnostics:\n")?;
				show_diagnostics(module_or_fail, s)?;
			}
			TestFailure::NoSuchBaseline(path) => {
				s.add("Baseline ")?.add(path)?.add(" does not yet exist.")?;
			},
			TestFailure::BaselineChanged { path, old, new } => {
				s.add("Baseline ")?.add(path)?.add(" has changed:\n")?;
				for diff in diff_lines(from_utf8(old).unwrap(), from_utf8(new).unwrap()) {
					match diff {
						Diff::Left(l) => s.add('-')?.add(l)?.nl()?,
						Diff::Both(l, _) => s.add(" ")?.add(l)?.nl()?,
						Diff::Right(r) => s.add('+')?.add(r)?.nl()?,
					};
				}
			},
			TestFailure::EmitError(ref e) => {
				s.add(e)?;
			},
		}
		Ok(())
	}
}
impl<'a> From<IoError> for TestFailure<'a> {
	fn from(error: IoError) -> Self {
		TestFailure::IoError(error)
	}
}
