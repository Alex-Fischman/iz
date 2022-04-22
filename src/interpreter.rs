use crate::compiler::Op;

pub fn interpret(program: &Vec<Op>) -> Vec<u8> {
	let mut stack = vec![];
	let pop_int = |stack: &mut Vec<u8>| {
		i64::from_be_bytes(
			stack.drain((stack.len() - 8)..).collect::<Vec<u8>>().try_into().unwrap(),
		)
	};

	let mut i = 0;
	while i < program.len() {
		match program[i] {
			Op::PushI(i) => stack.extend_from_slice(&i.to_be_bytes()),
			Op::NegI => {
				let a = pop_int(&mut stack);
				stack.extend_from_slice(&(-a).to_be_bytes());
			}
			Op::AddI => {
				let a = pop_int(&mut stack);
				let b = pop_int(&mut stack);
				stack.extend_from_slice(&(a + b).to_be_bytes());
			}
			Op::SubI => {
				let a = pop_int(&mut stack);
				let b = pop_int(&mut stack);
				stack.extend_from_slice(&(b - a).to_be_bytes());
			}
			Op::MulI => {
				let a = pop_int(&mut stack);
				let b = pop_int(&mut stack);
				stack.extend_from_slice(&(a * b).to_be_bytes());
			}
			Op::EqlI => {
				let a = pop_int(&mut stack);
				let b = pop_int(&mut stack);
				stack.push(u8::from(a == b));
			}
			Op::PushB(b) => stack.push(u8::from(b)),
			Op::EqlB => {
				let a = stack.pop().unwrap();
				let b = stack.pop().unwrap();
				stack.push(u8::from(a == b));
			}
		}
		i += 1;
	}
	stack
}
