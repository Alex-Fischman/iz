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

#[derive(Clone)]
pub enum Type {
	Data(Data),
	Generic(Vec<Constraint>),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Data(data) => write!(f, "{:?}", data),
			Type::Generic(constraints) => write!(f, "{:?}", constraints),
		}
	}
}

#[derive(Clone)]
pub struct Data {
	name: String,
	args: Vec<Type>,
}

impl Data {
	fn new(name: &str, args: Vec<Type>) -> Data {
		Data { name: name.to_string(), args }
	}
}

impl std::fmt::Debug for Data {
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

#[derive(Clone)]
pub enum Constraint {
	Named(String),
}

impl std::fmt::Debug for Constraint {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Constraint::Named(s) => write!(f, "{}", s),
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let int = || Type::Data(Data::new("int", vec![]));
	let boolean = || Type::Data(Data::new("bool", vec![]));
	let unit = || Type::Data(Data::new("unit", vec![]));
	let option = |a| Type::Data(Data::new("option", vec![a]));
	let func = |a, b| Type::Data(Data::new("func", vec![a, b]));
	let named = |s: &str| Type::Generic(vec![Constraint::Named(s.to_string())]);
	match ast {
		AST::Token(t) => Ok(TypedAST::Token(
			t.clone(),
			match &*t.string {
				s if s.chars().next().unwrap().is_numeric() => int(),
				"_iadd_" => func(int(), func(int(), int())),
				"_isub_" => func(int(), func(int(), int())),
				"_imul_" => func(int(), func(int(), int())),
				"_ineg_" => func(int(), int()),
				"true" => boolean(),
				"false" => boolean(),
				"_if_" => func(boolean(), func(named("a"), option(named("a")))),
				_ => Err(format!("unknown token: {:?}", t))?,
			},
		)),
		AST::Call(f, x) => {
			let f = annotate(f)?;
			let x = annotate(x)?;
			let f_type = f.get_type();
			match f_type {
				Type::Data(Data { name, args }) if name == "func" => {
					if &args[0] == x.get_type() {
						let y = args[1].clone();
						Ok(TypedAST::Call(Box::new(f), Box::new(x), y))
					} else {
						Err(format!(
							"argument type mismatch: {:?} != {:?}",
							args[0],
							x.get_type()
						))
					}
				}
				_ => Err(format!("expected function type: {:?}", f)),
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
					Some(t) => Ok(Type::Data(Data::new(
						"collection",
						typed_xs.iter().map(TypedAST::get_type).map(Clone::clone).collect(),
					))),
					None => Ok(Type::Generic(vec![])),
				},
				s => Err(format!("unknown bracket: {:?}", s)),
			}?;
			Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
		}
	}
}
