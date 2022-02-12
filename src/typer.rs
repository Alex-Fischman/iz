use crate::parser::AST;
use crate::tokenizer::Token;

#[derive(Debug)]
pub enum TypedAST {
	Token(Token, Type),
	List(Token, Vec<TypedAST>, Token, Type),
	Call(Box<TypedAST>, Box<TypedAST>, Type),
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
	Data(String, Vec<Type>),
	Func(Box<Type>, Box<Type>),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Func(x, y) => write!(f, "{:?}->{:?}", x, y),
			Type::Data(s, v) => {
				write!(f, "{}", s)?;
				for x in v {
					write!(f, "@{:?}", x)?;
				}
				Ok(())
			}
		}
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let data = |s: &str| Type::Data(s.to_string(), vec![]);
	let func = |x, y| Type::Func(Box::new(x), Box::new(y));
	let option = |a| Type::Data("option".to_string(), vec![a]);
	let var = |s: &str| data(s); // todo
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
		AST::List(a, xs, b) => match &*a.string {
			"(" => match xs.get(0) {
				Some(x) if xs.len() == 1 => Ok(annotate(x)?),
				_ => Err(format!("paren should only contain one expression")),
			},
			"{" => {
				let mut typed_xs = vec![];
				for x in xs {
					typed_xs.push(annotate(x)?);
				}
				let t = typed_xs.last().map(|x| x.get_type().clone()).unwrap_or(data("unit"));
				Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
			}
			"[" => todo!("arrays"),
			s => Err(format!("unknown bracket: {:?}", s)),
		},
		AST::Call(f, x) => {
			let f = annotate(f)?;
			match f.get_type().clone() {
				Type::Func(_a, b) => {
					let x = annotate(x)?;
					// todo: unify a and x to find b
					Ok(TypedAST::Call(Box::new(f), Box::new(x), *b.clone()))
				}
				t => Err(format!("expected function type but found {:?}", t)),
			}
		}
	}
}
