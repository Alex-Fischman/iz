use crate::compiler::Op;

pub fn interpret(program: &Vec<Op>) -> Vec<u8> {
	let mut stack = vec![];
	let pop_int = |stack: &mut Vec<u8>| {
		i64::from_be_bytes(
			stack.drain((stack.len() - 8)..).collect::<Vec<u8>>().try_into().unwrap(),
		)
	};
	for op in program {
		match op {
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
				stack.push((a == b) as u8);
			}
			Op::PushB(b) => stack.push(*b as u8),
			Op::EqlB => {
				let a = stack.pop();
				let b = stack.pop();
				stack.push((a == b) as u8);
			}
		}
	}
	stack
}
