use util::arena::Arena;
use util::sym::Sym;
use util::up::Up;

use model::class::{ClassHead, InstClass};
use model::method::MethodOrImplOrAbstract;

use super::inferrer::Inferrer;

pub fn try_get_method_of_inst_class<'infer, 'model>(
	inst_class: &InstClass<'model>,
	method_name: Sym,
	infer_arena: &'infer Arena,
	arena: &'model Arena,
) -> Option<MethodAndInferrer<'infer, 'model>> {
	try_get_method_of_inst_class_worker(
		inst_class,
		method_name,
		infer_arena,
		arena,
		/*is_super_search*/ false,
	)
}

fn try_get_method_of_inst_class_worker<'infer, 'model>(
	inst_class: &InstClass<'model>,
	method_name: Sym,
	infer_arena: &'infer Arena,
	model_arena: &'model Arena,
	is_super_search: bool,
) -> Option<MethodAndInferrer<'infer, 'model>> {
	//Remember to search both methods an impls.
 //Also need to search abstract methods if we *start* the search in an abstract class.

	for method in *inst_class.class.methods {
		if method.name() == method_name {
			let m = MethodOrImplOrAbstract::Method(Up(method));
			return Some(MethodAndInferrer(
				m,
				Inferrer::of_inst_class_and_method(inst_class, m, infer_arena, model_arena),
			))
		}
	}

	// If we just checked this class' subclass, no need to look for an abstract method,
 // because we would have found it as an Impl.
	if !is_super_search {
		if let ClassHead::Abstract(_, methods) = *inst_class.class.head {
			for method in methods {
				if method.name() == method_name {
					unimplemented!()
				}
			}
		}
	}

	for zuper in *inst_class.class.supers {
		//TODO:helper fn
		let got = try_get_method_of_inst_class_worker(
			&zuper.super_class,
			method_name,
			infer_arena,
			model_arena,
			/*is_super_search*/ true,
		);
		if got.is_some() {
			return got
		}
	}

	None
}

pub struct MethodAndInferrer<'infer, 'model: 'infer>(
	pub MethodOrImplOrAbstract<'model>,
	pub Inferrer<'infer, 'model>,
);
