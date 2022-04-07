use crate::scoper::Leaf;
use crate::typer::*;

#[derive(Clone, Debug)]
pub enum IR {
	Push(Frame),
	Add,
	Sub,
	Mul,
	Eql,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<IR>, String> {
	ast.walk(
		&mut |leaf, t| match (leaf, t) {
			(Leaf::Token(token), Type::List(s, xs, ())) if s == "int" && xs.is_empty() => {
				Ok(vec![IR::Push(Frame::Int(token.string.parse().unwrap()))])
			}
			(Leaf::Token(token), Type::List(s, xs, ())) if s == "bool" && xs.is_empty() => {
				match &*token.string {
					"true" => Ok(vec![IR::Push(Frame::Int(1))]),
					"false" => Ok(vec![IR::Push(Frame::Int(0))]),
					s => Err(format!("unknown bool value: {:?}", s))?,
				}
			}
			(_, data @ Type::List(_, _, ())) => Err(format!("unknown data type: {:?}", data))?,
			(Leaf::Token(token), t @ Type::Call(_, _, ())) => match &*token.string {
				"add" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Add]),
				"neg" if t == &func(int(), int()) => Ok(vec![IR::Push(Frame::Int(0)), IR::Sub]),
				"sub" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Sub]),
				"mul" if t == &func(int(), func(int(), int())) => Ok(vec![IR::Mul]),
				"eql" if t == &func(int(), func(int(), boolean())) => Ok(vec![IR::Eql]),
				"_if_" => todo!(),
				"_else_" => todo!(),
				s => Err(format!("unknown function {:?} with type {:?}", s, t))?,
			},
			(leaf, t) => Err(format!("not sure how to compile {:?}: {:?}", leaf, t))?,
		},
		&mut |l, xs, _| match l {
			crate::scoper::Lists::Paren => unreachable!(),
			crate::scoper::Lists::Curly => xs
				.into_iter()
				.collect::<Result<Vec<Vec<IR>>, _>>()
				.map(|xs: Vec<Vec<IR>>| xs.into_iter().flatten().collect()),
		},
		&mut |f, x, _| {
			let mut out = x?;
			out.extend(f?);
			Ok(out)
		},
	)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Frame {
	Int(i64),
	Bool(bool),
}

pub fn interpret(program: &[IR]) -> Vec<Frame> {
	let mut stack = vec![];
	let mut i = 0;
	while i < program.len() {
		if let IR::Push(data) = &program[i] {
			stack.push(data.clone());
		} else {
			match (&program[i], stack.pop().unwrap(), stack.pop().unwrap()) {
				(IR::Add, Frame::Int(a), Frame::Int(b)) => stack.push(Frame::Int(a + b)),
				(IR::Sub, Frame::Int(a), Frame::Int(b)) => stack.push(Frame::Int(a - b)),
				(IR::Mul, Frame::Int(a), Frame::Int(b)) => stack.push(Frame::Int(a * b)),
				(IR::Eql, a, b) => stack.push(Frame::Bool(a == b)),
				(op, a, b) => panic!("unknown op {:?} with args {:?} and {:?}", op, a, b),
			}
		}
		i += 1;
	}
	stack
}
