use util::arr::ArrBuilder;
use util::loc::Loc;

use super::super::diag::Diagnostic;
use super::super::model::effect::Effect;
use super::super::model::ty::{ Ty, InstCls };

use super::ty_replacer::TyReplacer;

pub fn get_common_compatible_type(a: &Ty, b: &Ty) -> Option<Ty> {
	match a {
		&Ty::Bogus =>
			Some(b.clone()),
		&Ty::Plain(effect_a, ref inst_cls_a) =>
			match b {
				&Ty::Bogus =>
					Some(a.clone()),
				&Ty::Plain(effect_b, ref inst_cls_b) => {
					if inst_cls_a.fast_equals(inst_cls_b) {
						Some(Ty::Plain(effect_a.min_common_effect(effect_b), inst_cls_a.clone()))
					} else {
						None
					}
				}
				&Ty::Param(_) =>
					todo!()
			},
		&Ty::Param(_) =>
			todo!()
	}
}

pub fn is_assignable(expected: &Ty, actual: &Ty) -> bool {
	match expected {
		&Ty::Bogus =>
			true,
		&Ty::Param(ref tpe) => {
			match actual {
				&Ty::Param(ref tpa) =>
					tpe.fast_equals(tpa),
				_ =>
					false,
			}
		}
		&Ty::Plain(effect_expected, ref inst_cls_expected) =>
			match actual {
				&Ty::Bogus =>
					true,
				&Ty::Plain(effect_actual, ref inst_cls_actual) =>
					effect_actual.contains(effect_expected) &&
						is_subclass(inst_cls_expected, inst_cls_actual),
				&Ty::Param(_) =>
					todo!(),
			},
	}
}

fn is_subclass(expected: &InstCls, actual: &InstCls) -> bool {
	// TODO: generics variance. Until then, only a subtype if every generic parameter is *exactly* equal.
	let &InstCls(ref expected_cls, ref expected_ty_arguments) = expected;
	let &InstCls(ref actual_cls, ref actual_ty_arguments) = actual;
	if expected_cls.ptr_equals(&actual_cls) && expected_ty_arguments.each_equals(&actual_ty_arguments, Ty::fast_equals) {
		return true
	}

	for s in actual_cls.supers().iter() {
		let instantiated_super_cls = instantiate_inst_cls(&s.super_class, &TyReplacer::of_inst_cls(actual));
		if is_subclass(expected, &instantiated_super_cls) {
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
		foo.console || This is a `get Console`, because we didn't explicitly specify an effect.
		foo.a || This is an `io Console`. Its type comes from the type parameter.
		foo.b || This is a `get MList[io Console]`. Don't narrow `io Console` because that was the type argument.
		foo.c || Forbidden. Can't narrow to pure `Console` because that would allow us to *add* a pure Console, but we must only add `io Console`s.

	The rule is, we always *either* instantiate a type parameter *xor* narrow an effect.
	*/
pub fn instantiate_ty_and_narrow_effects(
	narrowed_effect: Effect,
	ty: &Ty,
	replacer: &TyReplacer,
	loc: Loc,
	mut diags: &mut ArrBuilder<Diagnostic>) -> Ty {
	match ty {
		&Ty::Bogus =>
			Ty::Bogus,
		&Ty::Plain(original_effect, ref inst_cls) =>
			Ty::Plain(
				original_effect.min_common_effect(narrowed_effect),
				instantiate_inst_cls_and_forbid_effects(narrowed_effect, &inst_cls, &replacer, loc, &mut diags)),
		&Ty::Param(ref p) =>
			replacer.replace_or_same(p),
	}
}

pub fn narrow_effects(narrowed_effect: Effect, ty: &Ty, loc: Loc, diags: &mut ArrBuilder<Diagnostic>) -> Ty {
	instantiate_ty_and_narrow_effects(narrowed_effect, ty, &TyReplacer::do_nothing(), loc, diags)
}

fn instantiate_ty_and_forbid_effects(
	narrowed_effect: Effect,
	ty: &Ty,
	replacer: &TyReplacer,
	loc: Loc,
	mut diags: &mut ArrBuilder<Diagnostic>) -> Ty {
	match ty {
		&Ty::Bogus =>
			Ty::Bogus,
		&Ty::Plain(effect, ref inst_cls) =>
			if narrowed_effect.contains(effect) {
				Ty::Plain(effect, instantiate_inst_cls_and_forbid_effects(
					narrowed_effect, &inst_cls, &replacer, loc, &mut diags))
			} else {
				todo!()//diags.add(Diagnostic(loc, DiagnosticData::))
			},
		&Ty::Param(ref p) =>
			replacer.replace_or_same(p),
	}
}

fn instantiate_inst_cls_and_forbid_effects(
	narrowed_effect: Effect,
	inst_cls: &InstCls,
	replacer: &TyReplacer,
	loc: Loc,
	diags: &mut ArrBuilder<Diagnostic>) -> InstCls {
	map_inst_cls(inst_cls, |arg|
		instantiate_ty_and_forbid_effects(narrowed_effect, arg, replacer, loc, diags))
}

fn instantiate_inst_cls(inst_cls: &InstCls, replacer: &TyReplacer) -> InstCls {
	map_inst_cls(inst_cls, |arg| instantiate_ty(arg, &replacer))
}

fn map_inst_cls<F : FnMut(&Ty) -> Ty>(&InstCls(ref class_declaration, ref ty_arguments): &InstCls, replace_arg: F) -> InstCls {
	let new_ty_arguments = ty_arguments.map(replace_arg);
	InstCls(class_declaration.clone_ptr(), new_ty_arguments)
}

pub fn instantiate_ty(ty: &Ty, replacer: &TyReplacer) -> Ty {
	match ty {
		&Ty::Bogus =>
			Ty::Bogus,
		&Ty::Param(ref p) =>
			replacer.replace_or_same(&p),
		&Ty::Plain(effect, ref inst_cls) =>
			Ty::Plain(effect, instantiate_inst_cls(inst_cls, replacer)),
	}
}
