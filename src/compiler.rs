use crate::parser::Lists;
use crate::typer::{Type, TypedAST};
use std::io::{Error, ErrorKind};

#[derive(Debug, PartialEq)]
pub enum Op {
	PushI(i64),
	NegI,
	AddI,
	SubI,
	MulI,
}

pub fn compile(ast: &TypedAST) -> Result<Vec<Op>, Error> {
	Ok(match ast {
		TypedAST::Leaf(token, t) => match token.string.as_str() {
			"neg" if t == &(vec![Type::Int], vec![Type::Int]) => vec![Op::NegI],
			"add" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::AddI],
			"sub" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::SubI],
			"mul" if t == &(vec![Type::Int, Type::Int], vec![Type::Int]) => vec![Op::MulI],
			s if t == &(vec![], vec![Type::Int]) && s.chars().next().unwrap().is_numeric() => {
				vec![Op::PushI(s.parse::<i64>().unwrap())]
			}
			s => Err(Error::new(ErrorKind::Other, format!("unknown op {:?}", s)))?,
		},
		TypedAST::List(Lists::Block, xs, _) => {
			xs.iter().map(|x| compile(x)).flatten().flatten().collect()
		}
	})
}

pub fn interpret(program: &Vec<Op>) -> Vec<i64> {
	let mut stack = vec![];
	for op in program {
		match op {
			Op::PushI(int) => stack.push(*int),
			Op::NegI => {
				let a = stack.pop().unwrap();
				stack.push(-a);
			},
			Op::AddI => {
				let a = stack.pop().unwrap();
				let b = stack.pop().unwrap();
				stack.push(a + b);
			}
			Op::SubI => {
				let a = stack.pop().unwrap();
				let b = stack.pop().unwrap();
				stack.push(b - a);
			}
			Op::MulI => {
				let a = stack.pop().unwrap();
				let b = stack.pop().unwrap();
				stack.push(a * b);
			}
		}
	}
	stack
}
