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
	is_known: bool,
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

use Constraint::*;
#[derive(Debug)]
enum Constraint<'a> {
	EqualTo(Type),
	SameAsVarInOther(&'a str),
}

use std::collections::HashMap;
type VarMap<'a> = HashMap<&'a str, Vec<Constraint<'a>>>;

fn unify(a: &Type, b: &Type) -> Result<Type, String> {
	let mut a_vars = HashMap::new();
	let mut b_vars = HashMap::new();
	fn gather_constraints<'a>(
		a: &'a Type,
		b: &'a Type,
		a_vars: &mut VarMap<'a>,
		b_vars: &mut VarMap<'a>,
	) -> bool {
		match (a.is_known, b.is_known) {
			(true, true) if a.name != b.name => return false,
			(true, true) => {}
			(false, true) => {
				a_vars.entry(&a.name).or_insert(vec![]).push(EqualTo(b.clone()));
			}
			(true, false) => {
				b_vars.entry(&b.name).or_insert(vec![]).push(EqualTo(a.clone()));
			}
			(false, false) => {
				a_vars.entry(&a.name).or_insert(vec![]).push(SameAsVarInOther(&b.name));
				b_vars.entry(&b.name).or_insert(vec![]).push(SameAsVarInOther(&a.name));
			}
		}
		if a.args.len() != b.args.len() {
			return false;
		}
		a.args.iter().zip(b.args.iter()).all(|(a, b)| gather_constraints(a, b, a_vars, b_vars))
	}
	if !gather_constraints(a, b, &mut a_vars, &mut b_vars) {
		Err(format!("type {:?} is not the same as {:?}", a, b))
	} else {
		todo!("\na: {:?}\nb: {:?}\na_vars: {:?}\nb_vars: {:?}\n", a, b, a_vars, b_vars);
	}
}

pub fn annotate(ast: &AST) -> Result<TypedAST, String> {
	let var = |s: &str| Type { name: s.to_string(), args: vec![], is_known: false };
	let data = |s: &str| Type { name: s.to_string(), args: vec![], is_known: true };
	let option = |a| Type { name: "option".to_string(), args: vec![a], is_known: true };
	let array = |a| Type { name: "array".to_string(), args: vec![a], is_known: true };
	let func = |a, b| Type { name: "func".to_string(), args: vec![a, b], is_known: true };
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
			if f_type.name == "func" && f_type.is_known {
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
