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
	Data(String, Vec<Type>),
	Generic(String),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Generic(s) => write!(f, "{}", s),
			Type::Data(name, args) if name == "func" => write!(f, "{:?}->{:?}", args[0], args[1]),
			Type::Data(name, args) => {
				write!(f, "{}", name)?;
				for arg in args {
					write!(f, "@{:?}", arg)?;
				}
				Ok(())
			}
		}
	}
}

fn unify(a: &Type, b: &Type) -> Result<Type, String> {
	fn unify(
		a: &Type,
		b: &Type,
		a_vars: &mut std::collections::HashMap<String, Type>,
		b_vars: &mut std::collections::HashMap<String, Type>,
	) -> Result<Type, String> {
		match (a, b, a_vars, b_vars) {
			(Type::Data(a_name, a_args), Type::Data(b_name, b_args), a_vars, b_vars) => {
				if a_name == b_name {
					let mut args = vec![];
					for (a, b) in a_args.iter().zip(b_args) {
						args.push(unify(a, b, a_vars, b_vars)?);
					}
					Ok(Type::Data(a_name.clone(), args))
				} else {
					Err(format!("type mismatch: {:?} != {:?}", a, b))
				}
			}
			(Type::Generic(alias), data @ Type::Data(_, _), map, alt_map)
			| (data @ Type::Data(_, _), Type::Generic(alias), alt_map, map) => {
				let entry = map.entry(alias.to_string());
				entry.or_insert(data.clone());
				Ok(unify(&map.get(alias).unwrap().clone(), data, map, alt_map)?)
			}
			(Type::Generic(_a_alias), Type::Generic(_b_alias), a_vars, b_vars) => {
				todo!("\n{:#?}\n{:#?}\n{:#?}\n{:#?}", a, b, a_vars, b_vars)
			}
		}
	}
	unify(a, b, &mut std::collections::HashMap::new(), &mut std::collections::HashMap::new())
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let int = || Type::Data("int".to_string(), vec![]);
	let boolean = || Type::Data("bool".to_string(), vec![]);
	let unit = || Type::Data("unit".to_string(), vec![]);
	let option = |a| Type::Data("option".to_string(), vec![a]);
	let func = |a, b| Type::Data("func".to_string(), vec![a, b]);
	let named = |s: &str| Type::Generic(s.to_string());
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
				"_else_" => func(option(named("a")), func(named("a"), named("a"))),
				_ => Err(format!("unknown token: {:?}", t))?,
			},
		)),
		AST::Call(f, x) => {
			let f = annotate(f)?;
			let x = annotate(x)?;
			let f_type = f.get_type();
			match f_type {
				Type::Data(name, args) if name == "func" => {
					unify(&args[0], x.get_type())?;
					let y = args[1].clone();
					Ok(TypedAST::Call(Box::new(f), Box::new(x), y))
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
				"[" => Ok(Type::Data(
					"collection".to_string(),
					typed_xs.iter().map(TypedAST::get_type).map(Clone::clone).collect(),
				)),
				s => Err(format!("unknown bracket: {:?}", s)),
			}?;
			Ok(TypedAST::List(a.clone(), typed_xs, b.clone(), t))
		}
	}
}
