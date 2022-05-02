use crate::compiler::Op;

pub fn interpret(program: &Vec<Op>) -> Vec<u8> {
	let mut data_stack: Vec<u8> = vec![];
	let mut call_stack: Vec<i64> = vec![];
	let pop_int = |data_stack: &mut Vec<u8>| {
		i64::from_be_bytes(
			data_stack.drain((data_stack.len() - 8)..).collect::<Vec<u8>>().try_into().unwrap(),
		)
	};

	let mut ip: i64 = 0;
	while (ip as usize) < program.len() {
		match program[ip as usize] {
			Op::PushI(i) => data_stack.extend(&i.to_be_bytes()),
			Op::NegI => {
				let a = pop_int(&mut data_stack);
				data_stack.extend(&(-a).to_be_bytes());
			}
			Op::AddI => {
				let a = pop_int(&mut data_stack);
				let b = pop_int(&mut data_stack);
				data_stack.extend(&(a + b).to_be_bytes());
			}
			Op::SubI => {
				let a = pop_int(&mut data_stack);
				let b = pop_int(&mut data_stack);
				data_stack.extend(&(b - a).to_be_bytes());
			}
			Op::MulI => {
				let a = pop_int(&mut data_stack);
				let b = pop_int(&mut data_stack);
				data_stack.extend(&(a * b).to_be_bytes());
			}
			Op::EqlI => {
				let a = pop_int(&mut data_stack);
				let b = pop_int(&mut data_stack);
				data_stack.push(u8::from(a == b));
			}
			Op::PushB(b) => data_stack.push(u8::from(b)),
			Op::EqlB => {
				let a = data_stack.pop().unwrap();
				let b = data_stack.pop().unwrap();
				data_stack.push(u8::from(a == b));
			}
			Op::Jump => ip += pop_int(&mut data_stack),
			Op::Ip => data_stack.extend(&ip.to_be_bytes()),
			Op::Call => {
				call_stack.push(ip);
				ip = pop_int(&mut data_stack) - 1;
			}
			Op::Return => ip = call_stack.pop().unwrap(),
		}
		ip += 1;
	}
	assert!(call_stack.is_empty());
	data_stack
}
