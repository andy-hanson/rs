use util::up::Up;

use model::method::{MethodOrImpl, MethodWithBody};

use value::ValueCtx;

use super::emitted_model::{BuiltinCode, CalledBuiltin, CalledInstructions, Code, EmittedProgram, Fn1Args,
                           Fn2Args, Instruction, Instructions, LocAndInstruction};

pub fn run_method<'model, 'value, 'emit>(
	value_ctx: &'value mut ValueCtx<'model, 'value>,
	method: Up<'model, MethodWithBody<'model>>,
	emitted: &EmittedProgram<'model, 'emit>,
) -> () {
	assert!(method.is_static);
	assert_eq!(0, method.arity());
	let emitted_method = emitted.methods.get_method(method);
	exec(value_ctx, emitted_method, MethodOrImpl::Method(method))
}

fn exec<'model: 'value, 'emit, 'value>(
	ctx: &'value mut ValueCtx<'model, 'value>,
	first_code: &Code<'model, 'emit>,
	mut cur_method: MethodOrImpl<'model>,
) {
	let mut return_stack = Vec::<(MethodOrImpl, &'emit Instructions, usize)>::new();
	let mut cur_instructions: &Instructions<'model, 'emit> = match *first_code {
		Code::Instructions(ref i) => i,
		_ => unreachable!(),
	};
	let mut instruction_index = 0;

	loop {
		let LocAndInstruction(_, ref instruction) = cur_instructions.0[instruction_index];
		instruction_index += 1;
		match *instruction {
			Instruction::LiteralNat(n) => {
				let nat = ctx.nat(n);
				ctx.push(nat);
			}
			Instruction::LiteralInt(i) => {
				let int = ctx.int(i);
				ctx.push(int);
			}
			Instruction::LiteralFloat(f) => {
				let float = ctx.float(f);
				ctx.push(float);
			}
			Instruction::LiteralString(_) => unimplemented!(),
			Instruction::Fetch(n) => ctx.fetch(n),
			Instruction::UnLet(n) => ctx.un_let(n),
			Instruction::PopVoid => ctx.pop().assert_void(ctx),
			Instruction::CallInstructions(
				CalledInstructions(ref called_method, ref called_method_instructions),
			) => {
				return_stack.place_back() <- (cur_method, cur_instructions, instruction_index);
				cur_method = called_method.copy();
				cur_instructions = called_method_instructions;
				instruction_index = 0;
			}
			Instruction::CallBuiltin(CalledBuiltin(ref _called_method, ref builtin)) => {
				//TODO: use called_method for error reporting
				match *builtin {
					BuiltinCode::Fn0(f) => {
						let res = f(ctx);
						ctx.push(res);
					}
					BuiltinCode::Fn1(f) => {
						let a = ctx.pop();
						let res = f(Fn1Args(ctx, a));
						ctx.push(res);
					}
					BuiltinCode::Fn2(f) => {
						let a = ctx.pop();
						let b = ctx.pop();
						let res = f(Fn2Args(ctx, a, b));
						ctx.push(res);
					}
				}
			}
			Instruction::Return => {
				//pop parameters
				ctx.un_let(cur_method.arity());
				match return_stack.pop() {
					Some((method, instructions, index)) => {
						cur_method = method;
						cur_instructions = instructions;
						instruction_index = index
					}
					None => break,
				}
			}
			Instruction::NewSlots(ty, n) => ctx.pop_slots_and_push_new_value(ty, n),
			Instruction::GetSlot(offset) => {
				let obj = ctx.pop();
				ctx.push(obj.get_slot(offset))
			}
			Instruction::Assert => {
				let asserted = ctx.pop().as_bool(ctx);
				if !asserted {
					//TODO: noze exception
					unimplemented!()
				}
				let void = ctx.void();
				ctx.push(void)
			}
		}
	}

	assert_eq!(ctx.depth(), 1);
	let res = ctx.pop();
	res.assert_void(ctx)
}
