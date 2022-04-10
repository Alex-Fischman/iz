use crate::parser::*;
use std::io::{Error, ErrorKind};

pub enum TypedAST {
	Leaf(crate::tokenizer::Token, (Vec<Type>, Vec<Type>)),
	List(Lists, Vec<TypedAST>, (Vec<Type>, Vec<Type>)),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
	Int,
	Bool,
}

impl std::fmt::Debug for TypedAST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TypedAST::Leaf(token, t) => write!(f, "{:?}:{:?}", token, t),
			TypedAST::List(l, v, t) => write!(f, "{:?}{:#?}:{:?}", l, v, t),
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, Error> {
	match ast {
		AST::Leaf(token) => Ok(TypedAST::Leaf(
			token.clone(),
			match token.string.as_str() {
				s if s.chars().next().unwrap().is_numeric() => (vec![], vec![Type::Int]),
				"true" | "false" => (vec![], vec![Type::Bool]),
				"add" | "sub" | "mul" => (vec![Type::Int, Type::Int], vec![Type::Int]),
				"neg" => (vec![Type::Int], vec![Type::Int]),
				t => todo!("unknown token: {:?}", t),
			},
		)),
		AST::List(l, xs) => {
			let typed_xs: Result<Vec<TypedAST>, Error> = xs.iter().map(annotate).collect();
			let typed_xs = typed_xs?;
			let t = match l {
				Lists::Block => {
					let mut stack = vec![];
					for x in &typed_xs {
						let (input, output) = match x {
							TypedAST::Leaf(_, t) => t,
							TypedAST::List(_, _, t) => t,
						};
						let args = stack.split_off(stack.len() - input.len());
						if *input != args {
							Err(Error::new(
								ErrorKind::Other,
								format!("type error: {:?} != {:?}", input, args),
							))?
						}
						stack.extend_from_slice(&output);
					}
					(vec![], stack) // todo: recognize inputs while getting outputs
				}
				Lists::Group => todo!(),
			};
			Ok(TypedAST::List(*l, typed_xs, t))
		}
	}
}
