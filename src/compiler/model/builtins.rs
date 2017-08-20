use util::arr::Arr;
use util::loc::{ LOC_ZERO };
use util::ptr::Own;
use util::sym::Sym;

use super::class::{ ClassDeclaration, ClassHead };
use super::effect::Effect;
use super::method::{ MethodWithBody, Parameter };
use super::ty::{ Ty, InstCls };

/*
pub struct ClassDeclaration {
	pub name: Sym,
	pub type_parameters: Arr<Own<TypeParameter>>,
	_head: LateOwn<ClassHead>,
	_supers: LateOwn<Arr<Super>>,
	// Abstract methods are stored in the `head`
	_methods: LateOwn<Arr<Own<MethodWithBody>>>,
}
*/

fn foo() {
	/*let void = ClassDeclaration {
		name: Sym::of("Void"),
		type_parameters: Arr::empty(),
		_head: LateOwn::new(),

	}*/
	let void = ClassDeclaration::new(Sym::of("Void"), Arr::empty());
	void.set_head(ClassHead::Builtin);
	void.set_supers(Arr::empty());
	void.set_methods(Arr::empty());

	let bl = Own::new(ClassDeclaration::new(Sym::of("Bool"), Arr::empty()));
	bl.set_head(ClassHead::Builtin);
	bl.set_supers(Arr::empty());

	let bl_ty = Ty::pure_ty(InstCls(bl.ptr(), Arr::empty()));

	/*
	pub fn new(
		class: &Own<ClassDeclaration>, loc: Loc, is_static: bool, name: Sym,
		type_parameters: Arr<Own<TypeParameter>>, return_ty: Ty, self_effect: Effect,
		parameters: Arr<Own<Parameter>>) -> MethodWithBody
	*/
	/*pub struct Parameter { pub loc: Loc, pub ty: Ty, pub name: Sym, pub index: u32 }*/
	let bl_eq_param = Parameter { loc: LOC_ZERO, ty: bl_ty.clone(), name: Sym::of("!"), index: 0 };
	let bl_eq = MethodWithBody::new(&bl, LOC_ZERO, /*is_static*/ true, Sym::of("=="),
		/*type_parameters*/ Arr::empty(), /*return_ty*/ bl_ty.clone(), /*self_effect*/ Effect::Pure,
		/*parameters*/ Arr::_1(Own::new(bl_eq_param)));

	bl.set_methods(Arr::_1(Own::new(bl_eq)))

}


