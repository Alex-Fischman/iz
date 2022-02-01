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

#[derive(Clone, PartialEq)]
pub struct Type {
	name: String,
	size: usize,
	args: Vec<Type>,
}

impl Type {
	fn new(name: &str, size: usize, args: Vec<Type>) -> Type {
		Type { name: name.to_string(), size, args }
	}
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match &*self.name {
			"func" => write!(f, "{:?}->{:?}", self.args[0], self.args[1]),
			_ => {
				write!(f, "{}", self.name)?;
				if !self.args.is_empty() {
					write!(f, "@[{:?}", self.args[0])?;
					for n in &self.args[1..] {
						write!(f, ", {:?}", n)?;
					}
					write!(f, "]")?;
				}
				Ok(())
			}
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let int = || Type::new("int", 64, vec![]);
	let unit = || Type::new("unit", 0, vec![]);
	let unary_func = |a, b| Type::new("func", 64, vec![a, b]);
	let binary_func = |a, b, c| unary_func(a, unary_func(b, c));
	match ast {
		AST::Token(t) => Ok(TypedAST::Token(
			t.clone(),
			match &*t.string {
				s if s.chars().next().unwrap().is_numeric() => int(),
				"_iadd_" => binary_func(int(), int(), int()),
				"_isub_" => binary_func(int(), int(), int()),
				"_imul_" => binary_func(int(), int(), int()),
				"_ineg_" => unary_func(int(), int()),
				s => Err(format!("unknown token: {:?}", s))?,
			},
		)),
		AST::Call(f, x) => {
			let f = annotate(f)?;
			let x = annotate(x)?;
			if f.get_type().name == "func" {
				let a = &f.get_type().args[0];
				let b = f.get_type().args[1].clone();
				if a == x.get_type() {
					Ok(TypedAST::Call(Box::new(f), Box::new(x), b))
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
				"(" => match typed_xs.len() {
					1 => Ok(typed_xs[0].get_type().clone()),
					_ => Err(format!("paren should only contain one expression")),
				},
				"{" => Ok(match typed_xs.last() {
					Some(x) => x.get_type().clone(),
					None => unit(),
				}),
				"[" => match typed_xs.get(0).map(|x| x.get_type().clone()) {
					Some(t) => {
						if typed_xs.iter().all(|x| &t == x.get_type()) {
							Ok(Type::new("array", typed_xs.len() * t.size, vec![t]))
						} else {
							Err(format!("array was not heterogenous"))
						}
					}
					_ => todo!(),
				},
				s => Err(format!("unknown bracket: {:?}", s)),
			}?;
			Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
		}
	}
}
