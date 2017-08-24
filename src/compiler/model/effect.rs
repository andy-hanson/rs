#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Effect {
	Pure,
	Get,
	Set,
	Io,
}
impl Effect {
	pub fn contains(self, other: Effect) -> bool {
		self as usize > other as usize
	}

	pub fn min_common_effect(self, other: Effect) -> Effect {
		if self.contains(other) {
			self
		} else {
			other
		}
	}

	pub fn is_pure(self) -> bool {
		self == Effect::Pure
	}

	pub fn can_get(self) -> bool {
		self.contains(Effect::Get)
	}

	pub fn can_set(self) -> bool {
		self.contains(Effect::Set)
	}

	pub fn can_io(self) -> bool {
		self.contains(Effect::Io)
	}

	pub fn show(self) -> &'static str {
		match self {
			Effect::Pure => "pure",
			Effect::Get => "get",
			Effect::Set => "set",
			Effect::Io => "io",
		}
	}
}

pub const EFFECT_MIN: Effect = Effect::Pure;
pub const EFFECT_MAX: Effect = Effect::Io;
