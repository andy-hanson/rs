use util::arena::Arena;
use util::iter::KnownLen;
use util::late::Late;
use util::list::ListBuilder;
use util::loc::Loc;

use model::class::InstClass;
use model::diag::Diagnostic;
use model::effect::Effect;
use model::ty::{PlainTy, Ty};

use super::inferrer::{InferResult, InferrerLike};
use super::instantiator::{Instantiator, InstantiatorLike};

pub fn check_assignable<'model, I: InferrerLike<'model>>(
	expected: &Ty<'model>,
	actual: &Ty<'model>,
	inferrer: &I,
	arena: &'model Arena,
) -> bool {
	match *expected {
		Ty::Bogus =>
			// We already made a diagnostic.
			true,
		Ty::Param(param_expected) =>
			match inferrer.figure(param_expected, actual) {
				InferResult::JustFilledIn =>
					// Great, we inferred a type.
					true,
				InferResult::AlreadyHave(t) => {
					match *t {
						Ty::Bogus => true,
						Ty::Param(_) => unimplemented!(), //I think just return false?
						Ty::Plain(ref p) => check_plain_ty_assignable(p, actual, inferrer, arena),
					}
				}
				InferResult::NotInInferrer => {
					match *actual {
						Ty::Bogus => true,
						Ty::Param(param_actual) => param_expected.ptr_eq(param_actual),
						Ty::Plain(_) => false,
					}
				}
			}
		Ty::Plain(ref p) =>
			check_plain_ty_assignable(p, actual, inferrer, arena)
	}
}

fn check_plain_ty_assignable<'model, I: InferrerLike<'model>>(
	&PlainTy { effect: effect_expected, inst_class: ref inst_class_expected }: &PlainTy<'model>,
	actual: &Ty<'model>,
	inferrer: &I,
	arena: &'model Arena,
) -> bool {
	match *actual {
		Ty::Bogus => true,
		Ty::Param(_) =>
			//TODO: diagnostic
			unimplemented!(),
		Ty::Plain(PlainTy { effect: effect_actual, inst_class: ref inst_class_actual }) =>
			effect_actual.contains(effect_expected)
				&& check_class_assignable(inst_class_expected, inst_class_actual, inferrer, arena),
	}
}

fn check_class_assignable<'infer, 'model, I: InferrerLike<'model>>(
	expected: &InstClass<'model>,
	actual: &InstClass<'model>,
	inferrer: &I,
	arena: &'model Arena,
) -> bool {
	// TODO: generics variance.
 // Until then, only a subtype if every generic parameter is *exactly* equal.
	let &InstClass { class: expected_class, ty_args: expected_ty_args } = expected;
	let &InstClass { class: actual_class, ty_args: actual_ty_args } = actual;

	if expected_class.ptr_eq(actual_class) {
		//TODO: generics variance. Currently we are always invariant.
		return expected_ty_args.each_corresponds(actual_ty_args, |_expected_ty_arg, _actual_ty_arg| {
			let _ = inferrer; //TODO: use this
			unimplemented!()
		})
	}

	for _zuper in *actual_class.supers {
		let _ = arena;
		unimplemented!()
		//let instantiated_super_class =
  //	instantiate_inst_class(&zuper.super_class, &Instantiator::of_inst_class(actual), arena);
  //if check_class_assignable(expected, &instantiated_super_class, arena) {
  //	return true
  //}
	}

	false
}

/*
When accessing a slot, we need to simultaneously instantiate types *and* narrow effects.
Say we have:
	class Foo[T]
		slots
			val io Console console;
			val T a;
			val set MList[T] b;
			val MList[io Console] c;

	fun f(get Foo[io Console] foo)
        || This is a `get Console`, because we didn't explicitly specify an effect.
		foo.console
        || This is an `io Console`. Its type comes from the type parameter.
		foo.a
        || This is a `get MList[io Console]`.
		|| Don't narrow `io Console` because that was the type argument.
		foo.b
        || Forbidden.
		|| Can't narrow to pure `Console` because that would allow us to add a pure Console,
		|| but we must only add `io Console`s.
		foo.c

	The rule is, we always *either* instantiate a type parameter *xor* narrow an effect.
	*/
pub fn instantiate_and_narrow_effects<'model>(
	narrowed_effect: Effect,
	ty: &Ty<'model>,
	instantiator: &Instantiator<'model>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'model>>,
	arena: &'model Arena,
) -> Ty<'model> {
	match *ty {
		Ty::Bogus => Ty::Bogus,
		Ty::Plain(PlainTy { effect: original_effect, ref inst_class }) =>
			Ty::Plain(PlainTy {
				effect: original_effect.min_common_effect(narrowed_effect),
				inst_class: instantiate_inst_class_and_forbid_effects(
					narrowed_effect,
					inst_class,
					instantiator,
					loc,
					diags,
					arena,
				),
			}),
		Ty::Param(p) => instantiator.replace_or_same(p),
	}
}

pub fn narrow_effects<'model>(
	narrowed_effect: Effect,
	ty: &Ty<'model>,
	loc: Loc,
	diags: &mut ListBuilder<'model, Diagnostic<'model>>,
	arena: &'model Arena,
) -> Ty<'model> {
	instantiate_and_narrow_effects(narrowed_effect, ty, &Instantiator::NIL, loc, diags, arena)
}

fn instantiate_ty_and_forbid_effects<'model>(
	narrowed_effect: Effect,
	ty: &Ty<'model>,
	instantiator: &Instantiator<'model>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'model>>,
	arena: &'model Arena,
) -> Ty<'model> {
	match *ty {
		Ty::Bogus => Ty::Bogus,
		Ty::Plain(PlainTy { effect, ref inst_class }) =>
			if narrowed_effect.contains(effect) {
				Ty::Plain(PlainTy {
					effect,
					inst_class: instantiate_inst_class_and_forbid_effects(
						narrowed_effect,
						inst_class,
						instantiator,
						loc,
						diags,
						arena,
					),
				})
			} else {
				//TODO:Diagnostic
				unimplemented!()
			},
		Ty::Param(p) => instantiator.replace_or_same(p),
	}
}

fn instantiate_inst_class_and_forbid_effects<'a>(
	narrowed_effect: Effect,
	inst_class: &InstClass<'a>,
	instantiator: &Instantiator<'a>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'a>>,
	arena: &'a Arena,
) -> InstClass<'a> {
	map_inst_class(inst_class, arena, |arg| {
		instantiate_ty_and_forbid_effects(narrowed_effect, arg, instantiator, loc, diags, arena)
	})
}

fn instantiate_inst_class<'model, I: InstantiatorLike<'model>>(
	inst_class: &InstClass<'model>,
	instantiator: &I,
	arena: &'model Arena,
) -> InstClass<'model> {
	map_inst_class(inst_class, arena, |arg| instantiate_ty(arg, instantiator, arena))
}

fn map_inst_class<'a, F: FnMut(&Ty<'a>) -> Ty<'a>>(
	&InstClass { class, ty_args }: &InstClass<'a>,
	arena: &'a Arena,
	mut replace_ty_arg: F,
) -> InstClass<'a> {
	InstClass { class, ty_args: arena.map(ty_args, |t| Late::full(replace_ty_arg(t))) }
}

pub fn instantiate_ty<'model, I: InstantiatorLike<'model>>(
	ty: &Ty<'model>,
	instantiator: &I,
	arena: &'model Arena,
) -> Ty<'model> {
	match *ty {
		Ty::Bogus => Ty::Bogus,
		Ty::Param(p) => instantiator.replace_or_same(p),
		Ty::Plain(PlainTy { effect, ref inst_class }) => {
			let class = instantiate_inst_class(inst_class, instantiator, arena);
			Ty::Plain(PlainTy { effect, inst_class: class })
		}
	}
}
