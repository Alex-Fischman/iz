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

	fn set_type(&mut self, s: Type) {
		match self {
			TypedAST::Token(_, t) => *t = s,
			TypedAST::Call(_, _, t) => *t = s,
			TypedAST::List(_, _, _, t) => *t = s,
		}
	}
}

#[derive(Clone)]
pub struct Type {
	name: String,
	args: Vec<Type>,
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match &*self.name {
			"func" => write!(f, "{:?}->{:?}", self.args[0], self.args[1]),
			_ => {
				write!(f, "{}", self.name)?;
				for arg in &self.args {
					write!(f, "@{:?}", arg)?;
				}
				Ok(())
			}
		}
	}
}

fn unify(_a: &Type, _b: &Type) -> Result<Type, String> {
	todo!();
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let var = |s: &str| Type { name: s.to_string(), args: vec![]  };
	let data = |s: &str| Type { name: s.to_string(), args: vec![] };
	let option = |a| Type { name: "option".to_string(), args: vec![a] };
	let array = |a| Type { name: "array".to_string(), args: vec![a] };
	let func = |a, b| Type { name: "func".to_string(), args: vec![a, b] };
	match ast {
		AST::Token(t) => Ok(TypedAST::Token(
			t.clone(),
			match &*t.string {
				s if s.chars().next().unwrap().is_numeric() => data("int"),
				"_iadd_" => func(data("int"), func(data("int"), data("int"))),
				"_isub_" => func(data("int"), func(data("int"), data("int"))),
				"_imul_" => func(data("int"), func(data("int"), data("int"))),
				"_ineg_" => func(data("int"), data("int")),
				"true" => data("bool"),
				"false" => data("bool"),
				"_if_" => func(data("bool"), func(var("a"), option(var("a")))),
				"_else_" => func(option(var("a")), func(var("a"), var("a"))),
				_ => Err(format!("unknown token: {:?}", t))?,
			},
		)),
		AST::Call(f, x) => {
			let f = annotate(f)?;
			let x = annotate(x)?;
			let f_type = f.get_type();
			if f_type.name == "func" {
				unify(&f_type.args[0], x.get_type())?;
				let y = f_type.args[1].clone();
				Ok(TypedAST::Call(Box::new(f), Box::new(x), y))
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
					None => data("unit"),
				}),
				"[" => {
					if typed_xs.is_empty() {
						Ok(array(var("a")))
					} else {
						let mut arg = typed_xs[0].get_type().clone();
						for typed_x in &typed_xs {
							arg = unify(typed_x.get_type(), &arg)?;
						}
						for typed_x in &mut typed_xs {
							typed_x.set_type(arg.clone());
						}
						Ok(array(arg))
					}
				}
				s => Err(format!("unknown bracket: {:?}", s)),
			}?;
			Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
		}
	}
}
