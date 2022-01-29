use crate::parser::AST;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum TypedAST {
	Token(Token, Type),
	Call(Box<TypedAST>, Box<TypedAST>, Type),
	List(Token, Vec<TypedAST>, Token, Type),
}

impl TypedAST {
	fn get_type(&self) -> &Type {
		match self {
			TypedAST::Token(_, t) => t,
			TypedAST::Call(_, _, t) => t,
			TypedAST::List(_, _, _, t) => t,
		}
	}
}

use Type::*;
#[derive(Clone, PartialEq)]
pub enum Type {
	Unit,
	Int,
	Array(Box<Type>, usize),
	Func(Box<Type>, Box<Type>),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Unit => write!(f, "{{}}"),
			Int => write!(f, "Int"),
			Array(t, len) => write!(f, "[{:?}; {}]", t, len),
			Func(x, y) => write!(f, "{:?}->{:?}", x, y),
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	fn binary_func(a: Type, b: Type, c: Type) -> Type {
		Func(Box::new(a), Box::new(Func(Box::new(b), Box::new(c))))
	}
	match ast {
		AST::Token(t) => Ok(TypedAST::Token(
			t.clone(),
			match &*t.string {
				s if s.chars().next().unwrap().is_numeric() => Int,
				"_iadd_" => binary_func(Int, Int, Int),
				"_isub_" => binary_func(Int, Int, Int),
				"_imul_" => binary_func(Int, Int, Int),
				"_ineg_" => Func(Box::new(Int), Box::new(Int)),
				s => Err(format!("unknown token: {:?}", s))?,
			},
		)),
		AST::Call(f, x) => {
			let f = annotate(f)?;
			let x = annotate(x)?;
			if let Func(a, b) = f.get_type().clone() {
				if *a == *x.get_type() {
					Ok(TypedAST::Call(Box::new(f), Box::new(x), *b.clone()))
				} else {
					Err(format!("argument type mismatch: {:?} != {:?}", a, x.get_type()))
				}
			} else {
				Err(format!("expected function type: {:?}", f))
			}
		}
		AST::List(a, xs, b) => {
			let mut typed_xs = vec![];
			for x in xs {
				typed_xs.push(annotate(x)?);
			}
			let t = match &*a.string {
				"(" => {
					if typed_xs.len() == 1 {
						Ok(typed_xs[0].get_type().clone())
					} else {
						Err(format!("paren should only contain one expression"))
					}
				}
				"{" => Ok(match typed_xs.last() {
					Some(x) => x.get_type().clone(),
					None => Unit,
				}),
				"[" => match typed_xs.get(0).map(|x| x.get_type().clone()) {
					Some(t) => {
						if typed_xs.iter().all(|x| *x.get_type() == t) {
							Ok(Array(Box::new(t), typed_xs.len()))
						} else {
							Err(format!("array was not heterogenous"))
						}
					}
					None => todo!(),
				},
				s => Err(format!("unknown bracket: {:?}", s)),
			}?;
			Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
		}
	}
}
