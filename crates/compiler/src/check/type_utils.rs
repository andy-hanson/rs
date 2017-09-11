use util::arena::Arena;
use util::iter::KnownLen;
use util::list::ListBuilder;
use util::loc::Loc;
use util::up::ptr_eq;

use model::diag::Diagnostic;
use model::effect::Effect;
use model::ty::{InstCls, Ty};

use super::instantiator::Instantiator;

pub fn common_type<'a>(a: &Ty<'a>, b: &Ty<'a>) -> Option<Ty<'a>> {
	match *a {
		Ty::Bogus => Some(b.clone()),
		Ty::Plain(effect_a, ref inst_cls_a) =>
			match *b {
				Ty::Bogus => Some(a.clone()),
				Ty::Plain(effect_b, ref inst_cls_b) =>
					if inst_cls_a.fast_equals(inst_cls_b) {
						Some(Ty::Plain(effect_a.min_common_effect(effect_b), inst_cls_a.clone()))
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
				Ty::Param(tpa) => tpe.fast_equals(tpa),
				_ => false,
			},
		Ty::Plain(effect_expected, ref inst_cls_expected) =>
			match *actual {
				Ty::Bogus => true,
				Ty::Plain(effect_actual, ref inst_cls_actual) =>
					effect_actual.contains(effect_expected) &&
						is_subclass(inst_cls_expected, inst_cls_actual, arena),
				Ty::Param(_) => unimplemented!(),
			},
	}
}

fn is_subclass<'a>(expected: &InstCls<'a>, actual: &InstCls<'a>, arena: &'a Arena) -> bool {
	// TODO: generics variance.
	// Until then, only a subtype if every generic parameter is *exactly* equal.
	let &InstCls(expected_cls, expected_ty_arguments) = expected;
	let &InstCls(actual_cls, actual_ty_arguments) = actual;
	if ptr_eq(expected_cls, actual_cls) &&
		expected_ty_arguments.each_equals(actual_ty_arguments, Ty::fast_equals)
	{
		return true
	}

	for zuper in *actual_cls.supers {
		let instantiated_super_cls =
			instantiate_inst_cls(&zuper.super_class, &Instantiator::of_inst_cls(actual), arena);
		if is_subclass(expected, &instantiated_super_cls, arena) {
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
		Ty::Plain(original_effect, ref inst_cls) =>
			Ty::Plain(
				original_effect.min_common_effect(narrowed_effect),
				instantiate_inst_cls_and_forbid_effects(
					narrowed_effect,
					inst_cls,
					instantiator,
					loc,
					diags,
					arena,
				),
			),
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
		Ty::Plain(effect, ref inst_cls) =>
			if narrowed_effect.contains(effect) {
				Ty::Plain(
					effect,
					instantiate_inst_cls_and_forbid_effects(
						narrowed_effect,
						inst_cls,
						instantiator,
						loc,
						diags,
						arena,
					),
				)
			} else {
				//TODO:Diagnostic
				unimplemented!()
			},
		Ty::Param(p) => instantiator.replace_or_same(p),
	}
}

fn instantiate_inst_cls_and_forbid_effects<'a>(
	narrowed_effect: Effect,
	inst_cls: &InstCls<'a>,
	instantiator: &Instantiator<'a>,
	loc: Loc,
	diags: &mut ListBuilder<Diagnostic<'a>>,
	arena: &'a Arena,
) -> InstCls<'a> {
	map_inst_cls(inst_cls, arena, |arg| {
		instantiate_ty_and_forbid_effects(narrowed_effect, arg, instantiator, loc, diags, arena)
	})
}

fn instantiate_inst_cls<'a>(
	inst_cls: &InstCls<'a>,
	instantiator: &Instantiator<'a>,
	arena: &'a Arena,
) -> InstCls<'a> {
	map_inst_cls(inst_cls, arena, |arg| instantiate_type(arg, instantiator, arena))
}

fn map_inst_cls<'a, F: FnMut(&Ty<'a>) -> Ty<'a>>(
	&InstCls(decl, type_args): &InstCls<'a>,
	arena: &'a Arena,
	replace_type_arg: F,
) -> InstCls<'a> {
	let new_ty_arguments = arena.map(type_args, replace_type_arg);
	InstCls(decl, new_ty_arguments)
}

pub fn instantiate_type<'a>(ty: &Ty<'a>, instantiator: &Instantiator<'a>, arena: &'a Arena) -> Ty<'a> {
	match *ty {
		Ty::Bogus => Ty::Bogus,
		Ty::Param(p) => instantiator.replace_or_same(p),
		Ty::Plain(effect, ref inst_cls) => {
			let cls = instantiate_inst_cls(inst_cls, instantiator, arena);
			Ty::Plain(effect, cls)
		}
	}
}
