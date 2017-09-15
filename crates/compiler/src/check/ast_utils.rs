use ast;
use model::effect::Effect;

pub fn effect_to_effect(effect: ast::Effect) -> Effect {
	match effect {
		ast::Effect::Pure => Effect::Pure,
		ast::Effect::Get => Effect::Get,
		ast::Effect::Set => Effect::Set,
		ast::Effect::Io => Effect::Io,
	}
}
