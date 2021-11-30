use crate::env::Env;
use crate::parse::S;
use crate::token::Token;
use crate::token::TokenType;

use Expr::*;
#[derive(Clone)]
pub enum Expr {
	Unit,
	Bool(bool),
	Num(f64),
	Som(Box<Expr>),
	Non,
	Cons(Box<Expr>, Box<Expr>),
	Empty,
	Str(Token),
	Function(Token, S, Env, Option<Token>),
}

impl PartialEq for Expr {
	fn eq(&self, other: &Expr) -> bool {
		match (self, other) {
			(Unit, Unit) => true,
			(Bool(a), Bool(b)) => a == b,
			(Num(a), Num(b)) => a == b,
			(Som(a), Som(b)) => a == b,
			(Non, Non) => true,
			(Cons(a, b), Cons(c, d)) => a == c && b == d,
			(Empty, Empty) => true,
			(Str(a), Str(b)) => a.string == b.string,
			_ => false,
		}
	}
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Unit => write!(f, "{{}}"),
			Bool(b) => write!(f, "{}", b),
			Num(n) => write!(f, "{}", n),
			Som(e) => write!(f, "Some({:?})", e),
			Non => write!(f, "None"),
			Cons(a, b) => {
				write!(f, "[{:?}", a)?;
				let mut e = b;
				while let Cons(a, b) = &**e {
					write!(f, " {:?}", a)?;
					e = &b;
				}
				write!(f, "]")
			}
			Empty => write!(f, "[]"),
			Str(s) => write!(f, "{}", s.string),
			Function(x, y, _, _) => write!(f, "{:?}->{:?}", x, y),
		}
	}
}

pub fn interpret(s: &S, c: &mut Env) -> Expr {
	match s.value.t {
		TokenType::Numeric => return Expr::Num(s.value.string.parse().unwrap()),
		TokenType::Quote => return Expr::Str(s.value.clone()),
		_ => {}
	}

	match &*s.value.string {
		"(" => return interpret(&s.children[0], c),
		"{" => {
			let mut new_scope = c.clone();
			return s.children.iter().fold(Expr::Unit, |_, s| interpret(s, &mut new_scope));
		}
		"[" => {
			return s.children.iter().rev().fold(Expr::Empty, |acc, s| {
				Expr::Cons(Box::new(interpret(s, c)), Box::new(acc))
			})
		}
		"false" => return Expr::Bool(false),
		"true" => return Expr::Bool(true),
		"None" => return Expr::Non,
		"call" => {}
		o => return c.get_var(o).unwrap_or_else(|| panic!("unknown variable {}\n{:?}", o, c)),
	}

	if s.children.len() == 2 {
		match s.children[0].value.string.as_str() {
			"Some" => return Expr::Som(Box::new(interpret(&s.children[1], c))),
			"print" => {
				println!("{:?}", interpret(&s.children[1], c));
				return Expr::Unit;
			}
			_ => {}
		}
	}

	if s.children.len() == 2 && s.children[0].children.len() == 2 {
		let get_a = |c: &mut Env| interpret(&s.children[0].children[1], c);
		let get_b = |c: &mut Env| interpret(&s.children[1], c);

		let mut get_num_bin_op = |f: fn(f64, f64) -> Expr| match (get_a(c), get_b(c)) {
			(Expr::Num(a), Expr::Num(b)) => f(a, b),
			(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
		};

		match s.children[0].children[0].value.string.as_str() {
			"assert" => {
				if get_a(c) != get_b(c) {
					panic!(
						"{:?} != {:?} @ {:?}\n{:?}",
						get_a(c),
						get_b(c),
						s.value.pos,
						c
					);
				}
				return Expr::Unit;
			}
			"func" => {
				return Expr::Function(
					s.children[0].children[1].value.clone(),
					s.children[1].clone(),
					c.clone(),
					None,
				)
			}
			"if_" => match interpret(&s.children[0].children[1], c) {
				Expr::Bool(true) => return Expr::Som(Box::new(get_b(c))),
				Expr::Bool(false) => return Expr::Non,
				a => panic!("{:?} {:?}\n{:?}", a, s, c),
			},
			"else_" => match get_a(c) {
				Expr::Som(a) => return *a,
				Expr::Non => return get_b(c),
				a => panic!("{:?} {:?}\n{:?}", a, s, c),
			},
			"set" => {
				let mut e = get_b(c);
				if let Expr::Function(_, _, _, n) = &mut e {
					*n = Some(s.children[0].children[1].value.clone());
				}
				c.add_var(s.children[0].children[1].value.clone(), e);
				return Expr::Unit;
			}
			"try" => {
				fn destruct(s: &S, a: Expr, c: &mut Env) -> bool {
					match s.value.string.as_str() {
						"call"
							if s.children[0].children.len() > 0
								&& s.children[0].children[0].value.string == "cons" =>
						{
							match a {
								Expr::Cons(a, b) => {
									destruct(&s.children[0].children[1], *a, c)
										&& destruct(&s.children[1], *b, c)
								}
								_ => false,
							}
						}
						"call" if s.children[0].value.string == "Some" => match a {
							Expr::Som(a) => destruct(&s.children[1], *a, c),
							_ => false,
						},
						"_" => true,
						_ => {
							c.push_var(s.value.clone(), a);
							true
						}
					}
				}
				return Expr::Bool(destruct(&s.children[0].children[1], get_b(c), c));
			}
			"cons" => return Expr::Cons(Box::new(get_a(c)), Box::new(get_b(c))),
			"eq" => return Expr::Bool(get_a(c) == get_b(c)),
			"gt" => return get_num_bin_op(|a, b| Expr::Bool(a > b)),
			"lt" => return get_num_bin_op(|a, b| Expr::Bool(a < b)),
			"add" => return get_num_bin_op(|a, b| Expr::Num(a + b)),
			"sub" => return get_num_bin_op(|a, b| Expr::Num(a - b)),
			"mul" => return get_num_bin_op(|a, b| Expr::Num(a * b)),
			"div" => return get_num_bin_op(|a, b| Expr::Num(a / b)),
			"mod" => return get_num_bin_op(|a, b| Expr::Num(a % b)),
			"pow" => return get_num_bin_op(|a, b| Expr::Num(a.powf(b))),
			_ => {}
		}
	}

	match interpret(&s.children[0], c) {
		Expr::Function(x, y, mut z, n) => {
			if let Some(n) = n {
				z.push_var(
					n.clone(),
					Expr::Function(x.clone(), y.clone(), z.clone(), Some(n)),
				);
			}
			z.push_var(x, interpret(&s.children[1], c));
			interpret(&y, &mut z)
		}
		o => panic!("{:?} {:?}\n{:?}", o, s, c),
	}
}
