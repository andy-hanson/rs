use super::value::Value;

pub enum Instruction {
	/** Push a literal value onto the stack. */
	Literal(Value),
	/** Fetch a value from N values up the stack. */
	Fetch(u32),
	/**
	Pop N values out from the top value on the stack.
	Leaves the top value alone.
	*/
	UnLet(u32),
	/** Pops the `Void` value pushed by the last expression. */
	PopVoid,
}
