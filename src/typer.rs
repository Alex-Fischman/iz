use crate::parser::*;
use std::io::{Error, ErrorKind};

#[derive(PartialEq)]
pub enum TypedAST {
	Leaf(crate::tokenizer::Token, (Vec<Type>, Vec<Type>)),
	List(Lists, Vec<TypedAST>, (Vec<Type>, Vec<Type>)),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
	Int,
	Bool,
	Var(usize),
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
	fn annotate(ast: &AST, vars: &mut Vec<Option<Type>>) -> Result<TypedAST, Error> {
		match ast {
			AST::Leaf(token) => Ok(TypedAST::Leaf(
				token.clone(),
				match token.string.as_str() {
					s if s.chars().next().unwrap().is_numeric() => (vec![], vec![Type::Int]),
					"true" | "false" => (vec![], vec![Type::Bool]),
					"add" | "sub" | "mul" => (vec![Type::Int, Type::Int], vec![Type::Int]),
					"eql" => (
						vec![
							Type::Var({
								vars.push(None);
								vars.len() - 1
							}),
							Type::Var(vars.len() - 1),
						],
						vec![Type::Bool],
					),
					"neg" => (vec![Type::Int], vec![Type::Int]),
					"branch" => (vec![Type::Bool, Type::Int], vec![]),
					t => Err(Error::new(ErrorKind::Other, format!("unknown token {:?}", t)))?,
				},
			)),
			AST::List(l, xs) => {
				let typed_xs: Result<Vec<TypedAST>, Error> =
					xs.iter().map(|x| annotate(x, vars)).collect();
				let typed_xs = typed_xs?;
				let t = match l {
					Lists::Block => {
						let mut input_stack: Vec<Type> = vec![];
						let mut output_stack: Vec<Type> = vec![];
						for x in &typed_xs {
							let (input, output) = match x {
								TypedAST::Leaf(_, t) => t,
								TypedAST::List(_, _, t) => t,
							};
							let mut input = input.clone();
							if input.len() > output_stack.len() {
								let to_remove = input.len() - output_stack.len();
								input_stack.extend(input.drain(..to_remove));
							}
							let args = output_stack.split_off(output_stack.len() - input.len());
							for (arg, input) in args.iter().zip(input) {
								match (arg, input) {
									(arg, Type::Var(v)) if vars[v] == None => {
										vars[v] = Some(arg.clone());
									}
									(arg, Type::Var(v)) if vars[v].as_ref() == Some(arg) => {}
									(arg, Type::Var(v)) => Err(Error::new(
										ErrorKind::Other,
										format!(
											"type error: {:?} != {:?}",
											arg,
											vars[v].as_ref().unwrap()
										),
									))?,
									(a, b) if a == &b => {}
									(a, b) => Err(Error::new(
										ErrorKind::Other,
										format!("type error: {:?} != {:?}", a, b),
									))?,
								}
							}
							output_stack.extend_from_slice(&output);
						}
						(input_stack, output_stack)
					}
				};
				Ok(TypedAST::List(*l, typed_xs, t))
			}
		}
	}

	let mut vars = vec![];
	let mut out = annotate(ast, &mut vars)?;
	for var in &vars {
		if var.is_none() {
			Err(Error::new(ErrorKind::Other, format!("type error: unsolved var")))?
		}
	}
	fn replace_vars(ast: &mut TypedAST, vars: &Vec<Type>) {
		if let TypedAST::List(_, xs, _) = ast {
			for x in xs {
				replace_vars(x, vars);
			}
		}
		let (input, output) = &mut match ast {
			TypedAST::Leaf(_, t) => t,
			TypedAST::List(_, _, t) => t,
		};
		for t in input.iter_mut().chain(output.iter_mut()) {
			if let Type::Var(v) = t {
				*t = vars[*v].clone();
			}
		}
	}
	replace_vars(&mut out, &vars.into_iter().map(Option::unwrap).collect());
	Ok(out)
}
