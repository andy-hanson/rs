use util::arena::{Arena, NoDrop};
use util::file_utils::read_file;
use util::iter::KnownLen;
use util::late::Late;
use util::list::List;
use util::path::Path;
use util::sym::Sym;
use util::sync::UnsafeSync;
use util::up::Up;

use util::string_maker::{Shower, WriteShower};

use model::class::ClassDeclaration;
use model::diag::{Diag, Diagnostic};
use model::diag::show_diagnostics;
use model::module::{FailModule, Module, ModuleOrFail, ModuleSourceEnum};
use model::ty::{InstCls, Ty};

use super::check::check_module;
use super::parse::ast::Module as ModuleAst;
use super::parse::parse;

lazy_static! {
	static ref BUILTINS_ARENA: UnsafeSync<Arena> = UnsafeSync(Arena::new());
	static ref BUILTINS_FILES: [(Sym, &'static [u8]); 6] =
		[
			(Sym::of("Void"), load_builtin_file(b"builtins/Void.nz")),
			(Sym::of("Bool"), load_builtin_file(b"builtins/Bool.nz")),
			(Sym::of("Int"), load_builtin_file(b"builtins/Int.nz")),
			(Sym::of("Nat"), load_builtin_file(b"builtins/Nat.nz")), // Nat - Nat is Int, so Nat must come after Int
			(Sym::of("Float"), load_builtin_file(b"builtins/Float.nz")),
			(Sym::of("String"), load_builtin_file(b"builtins/String.nz")),
		];
}

fn load_builtin_file(path_slice: &[u8]) -> &[u8] {
	let path = Path::of_slice(path_slice);
	let arena = &BUILTINS_ARENA;
	match read_file(path, arena.get()).unwrap() {
		Some(b) => b,
		None => {
			let str = String::from_utf8(path.slice().to_owned()).unwrap();
			panic!("Can't load builtin from {}", str)
		}
	}
}

pub struct BuiltinsOwn<'model> {
	pub all: Late<&'model [ModuleOrFail<'model>]>,
	pub all_successes: Late<&'model [Up<'model, Module<'model>>]>,
	pub void: Late<Ty<'model>>,
	pub bool: Late<Ty<'model>>,
}
impl<'model> NoDrop for BuiltinsOwn<'model> {}
impl<'model> BuiltinsOwn<'model> {
	#[allow(needless_lifetimes)] // Can't seem to write this without the lifetimes?
	pub fn as_ctx(&'model self) -> BuiltinsCtx<'model> {
		BuiltinsCtx { all_successes: *self.all_successes, void: Some(&self.void), bool: Some(&self.bool) }
	}
}

pub struct BuiltinsCtx<'model> {
	pub all_successes: &'model [Up<'model, Module<'model>>],
	pub void: Option<&'model Ty<'model>>,
	pub bool: Option<&'model Ty<'model>>,
}

pub fn get_builtins(arena: &Arena) -> &BuiltinsOwn {
	let mut all_successes = arena.max_len_builder(BUILTINS_FILES.len());
	let own = arena <- BuiltinsOwn {
		all: Late::new(),
		all_successes: Late::new(),
		void: Late::new(),
		bool: Late::new(),
	};
	let sym_void = Sym::of("void");
	let sym_bool = Sym::of("bool");

	//TODO: let all = BUILTINS_FILES.map(arena, |&(name, text)| {
	let mut all = arena.exact_len_builder(BUILTINS_FILES.len());
	for &(name, text) in &*BUILTINS_FILES {
		//println!("COMPILING: {:?}", name);
		let source = ModuleSourceEnum::Builtin { name, text };
		let ast_arena = Arena::new();
		&mut all <- match parse(&ast_arena, text) {
			Ok(ModuleAst { imports, class }) => {
				let module = arena <- Module {
					source,
					imports: &[],
					class: Late::new(),
					diagnostics: Late::new(),
				};
				assert!(imports.is_empty());
				{
					let cur_builtins = BuiltinsCtx {
						all_successes: all_successes.slice_so_far(),
						void: own.void.try_get(),
						bool: own.bool.try_get(),
					};
					check_module(module, &cur_builtins, class, name, arena);
				}
				if name == sym_void {
					&own.void <- primitive_ty(&module.class);
				} else if name == sym_bool {
					&own.bool <- primitive_ty(&module.class);
				}
				&mut all_successes <- Up(module);
				ModuleOrFail::Module(module)
			}
			Err((loc, parse_diag)) => {
				let diag = Diagnostic { loc, diag: Diag::ParseError(parse_diag) };
				let mf = ModuleOrFail::Fail(
					arena <- FailModule { source, imports: &[], diagnostics: List::single(diag, arena) },
				);
				let mut shower = WriteShower::stderr();
				show_diagnostics(&mf, &mut shower).unwrap();
				shower.nl().unwrap();
				panic!("Bai");
				//TODO: just return it...
			}
		};
	}

	&own.all <- all.finish();
	&own.all_successes <- all_successes.finish();

	own
}

fn primitive_ty<'model>(cls: &'model ClassDeclaration<'model>) -> Ty<'model> {
	Ty::pure_ty(InstCls(cls, &[]))
}
