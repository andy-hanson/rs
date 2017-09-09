use util::arena::NoDrop;

#[derive(Copy, Clone, Eq, PartialEq, Serialize)]
pub enum Effect {
	Pure,
	Get,
	Set,
	Io,
}
impl NoDrop for Effect {}
impl Effect {
	pub const MIN: Self = Effect::Pure;
	pub const MAX: Self = Effect::Io;

	pub fn contains(self, other: Self) -> bool {
		self as usize > other as usize
	}

	pub fn min_common_effect(self, other: Self) -> Self {
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
