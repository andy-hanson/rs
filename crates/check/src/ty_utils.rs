use util::arena::Arena;
use util::iter::KnownLen;
use util::list::ListBuilder;
use util::loc::Loc;

use model::class::InstClass;
use model::diag::Diagnostic;
use model::effect::Effect;
use model::ty::{PlainTy, Ty};

use super::instantiator::Instantiator;

pub fn common_ty<'a>(a: &Ty<'a>, b: &Ty<'a>) -> Option<Ty<'a>> {
	match *a {
		Ty::Bogus => Some(b.clone()),
		Ty::Plain(PlainTy { effect: effect_a, inst_class: ref inst_class_a }) =>
			match *b {
				Ty::Bogus => Some(a.clone()),
				Ty::Plain(PlainTy { effect: effect_b, inst_class: ref inst_class_b }) =>
					if inst_class_a.fast_equals(inst_class_b) {
						Some(Ty::Plain(PlainTy {
							effect: effect_a.min_common_effect(effect_b),
							inst_class: inst_class_a.clone(),
						}))
					} else {
						None
					},
				Ty::Param(_) => unimplemented!(),
			},
		Ty::Param(_) => unimplemented!(),
	}
}

pub fn is_assignable<'a>(expected: &Ty<'a>, actual: &Ty<'a>, arena: &'a Arena) -> bool {
	match *expected {
		Ty::Bogus => true,
		Ty::Param(tpe) =>
			match *actual {
				Ty::Param(tpa) => tpe.ptr_eq(tpa),
				_ => false,
			},
		Ty::Plain(PlainTy { effect: effect_expected, inst_class: ref inst_class_expected }) =>
			match *actual {
				Ty::Bogus => true,
				Ty::Plain(PlainTy { effect: effect_actual, inst_class: ref inst_class_actual }) =>
					effect_actual.contains(effect_expected)
						&& is_subclass(inst_class_expected, inst_class_actual, arena),
				Ty::Param(_) => unimplemented!(),
			},
	}
}

fn is_subclass<'a>(expected: &InstClass<'a>, actual: &InstClass<'a>, arena: &'a Arena) -> bool {
	// TODO: generics variance.
 // Until then, only a subtype if every generic parameter is *exactly* equal.
	let &InstClass { class: expected_class, ty_args: expected_ty_args } = expected;
	let &InstClass { class: actual_class, ty_args: actual_ty_args } = actual;
	if expected_class.ptr_eq(actual_class) && expected_ty_args.each_equals(actual_ty_args, Ty::fast_equals) {
		return true
	}

	for zuper in *actual_class.supers {
		let instantiated_super_class =
			instantiate_inst_class(&zuper.super_class, &Instantiator::of_inst_class(actual), arena);
		if is_subclass(expected, &instantiated_super_class, arena) {
			return true
		}
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
pub fn instantiate_and_narrow_effects<'a>(
	narrowed_effect: Effect,
	ty: &Ty<'a>,
	instantiator: &Instantiator<'a>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'a>>,
	arena: &'a Arena,
) -> Ty<'a> {
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

pub fn narrow_effects<'a>(
	narrowed_effect: Effect,
	ty: &Ty<'a>,
	loc: Loc,
	diags: &mut ListBuilder<'a, Diagnostic<'a>>,
	arena: &'a Arena,
) -> Ty<'a> {
	instantiate_and_narrow_effects(narrowed_effect, ty, &Instantiator::NIL, loc, diags, arena)
}

fn instantiate_ty_and_forbid_effects<'a>(
	narrowed_effect: Effect,
	ty: &Ty<'a>,
	instantiator: &Instantiator<'a>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'a>>,
	arena: &'a Arena,
) -> Ty<'a> {
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

fn instantiate_inst_class<'a>(
	inst_class: &InstClass<'a>,
	instantiator: &Instantiator<'a>,
	arena: &'a Arena,
) -> InstClass<'a> {
	map_inst_class(inst_class, arena, |arg| instantiate_ty(arg, instantiator, arena))
}

fn map_inst_class<'a, F: FnMut(&Ty<'a>) -> Ty<'a>>(
	&InstClass { class, ty_args }: &InstClass<'a>,
	arena: &'a Arena,
	replace_ty_arg: F,
) -> InstClass<'a> {
	InstClass { class, ty_args: arena.map(ty_args, replace_ty_arg) }
}

pub fn instantiate_ty<'a>(ty: &Ty<'a>, instantiator: &Instantiator<'a>, arena: &'a Arena) -> Ty<'a> {
	match *ty {
		Ty::Bogus => Ty::Bogus,
		Ty::Param(p) => instantiator.replace_or_same(p),
		Ty::Plain(PlainTy { effect, ref inst_class }) => {
			let class = instantiate_inst_class(inst_class, instantiator, arena);
			Ty::Plain(PlainTy { effect, inst_class: class })
		}
	}
}
