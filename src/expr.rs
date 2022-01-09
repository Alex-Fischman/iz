use crate::token::Token;
use crate::token::TokenType;
use crate::parse::S;

use Expr::*;
#[derive(Clone)]
enum Expr {
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
			Function(x, y, _, _) => write!(f, "{}->{:?}", x.string, y),
		}
	}
}

#[derive(Clone)]
struct Env {
	head: Link,
}

struct Node {
	next: Link,
	lhs: Token,
	rhs: Expr,
}

type Link = Option<std::rc::Rc<std::cell::RefCell<Node>>>;

impl Env {
	fn new() -> Env {
		Env { head: None }
	}

	fn get_var(&self, s: &str) -> Option<Expr> {
		fn get_var(n: &Node, s: &str) -> Option<Expr> {
			if n.lhs.string == s {
				Some(n.rhs.clone())
			} else {
				n.next.as_ref().and_then(|n| get_var(&n.borrow(), s))
			}
		}

		self.head.as_ref().and_then(|n| get_var(&n.borrow(), s))
	}

	fn add_var(&mut self, lhs: Token, rhs: Expr) {
		fn add_var(n: &mut Node, lhs: Token, rhs: Expr) -> bool {
			if lhs.string == n.lhs.string {
				n.lhs = lhs;
				n.rhs = rhs;
				false
			} else if let Some(n) = &n.next {
				add_var(&mut n.borrow_mut(), lhs, rhs)
			} else {
				true
			}
		}

		if let Some(h) = &self.head {
			if add_var(&mut h.borrow_mut(), lhs.clone(), rhs.clone()) {
				panic!("could not find var {:?} in\n{:?}", lhs.string, self);
			}
		} else {
			panic!("could not find var {:?} in\n{:?}", lhs.string, self);
		}
	}

	fn push_var(&mut self, lhs: Token, rhs: Expr) {
		self.head = Some(std::rc::Rc::new(std::cell::RefCell::new(Node {
			next: self.head.clone(),
			lhs,
			rhs,
		})))
	}
}

impl std::fmt::Debug for Env {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		fn fmt(n: &Node, f: &mut std::fmt::Formatter) -> std::fmt::Result {
			if let Some(n) = &n.next {
				fmt(&n.borrow(), f)?;
			}
			if let Some(p) = &n.lhs.pos {
				write!(f, "{:?} \t", p)?;
			}
			writeln!(f, "{}: {:?}", n.lhs.string, n.rhs)
		}

		match &self.head {
			Some(n) => fmt(&n.borrow(), f),
			None => writeln!(f, "empty env"),
		}
	}
}

fn destruct(s: &S, a: Expr, c: &mut Env) -> bool {
	match s.value.string.as_str() {
		"call"
			if s.children[0].children.len() > 0
				&& s.children[0].children[0].value.string == "cons" =>
		{
			match a {
				Cons(a, b) => {
					destruct(&s.children[0].children[1], *a, c)
						&& destruct(&s.children[1], *b, c)
				}
				_ => false,
			}
		}
		"call" if s.children[0].value.string == "Some" => match a {
			Som(a) => destruct(&s.children[1], *a, c),
			_ => false,
		},
		"_" => true,
		_ => {
			c.push_var(s.value.clone(), a);
			true
		}
	}
}

pub fn interpret(s: &S) {
	fn interpret(s: &S, c: &mut Env) -> Expr {
		match s.value.t {
			TokenType::Numeric => Num(s.value.string.parse().unwrap()),
			TokenType::Quote => Str(s.value.clone()),
			_ => match s.value.string.as_str() {
				"(" => interpret(&s.children[0], c),
				"{" => {
					let mut new_scope = c.clone();
					s.children.iter().fold(Unit, |_, s| interpret(s, &mut new_scope))
				}
				"[" => s.children.iter().rev().fold(Empty, |acc, s| {
					Cons(Box::new(interpret(s, c)), Box::new(acc))
				}),
				"false" => Bool(false),
				"true" => Bool(true),
				"None" => Non,
				"call" => {
					if let Some(e) = match s.children[0].value.string.as_str() {
						"Some" => Some(Som(Box::new(interpret(&s.children[1], c)))),
						"print" => {
							println!("{:?}", interpret(&s.children[1], c));
							Some(Unit)
						}
						_ => None,
					} {
						e
					} else if let Some(e) = if s.children[0].children.len() == 2 {
						let mut get_num_bin_op = |f: fn(f64, f64) -> Expr| match (
							interpret(&s.children[0].children[1], c),
							interpret(&s.children[1], c),
						) {
							(Num(a), Num(b)) => Some(f(a, b)),
							(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
						};

						match s.children[0].children[0].value.string.as_str() {
							"assert" => {
								if interpret(&s.children[0].children[1], c)
									!= interpret(&s.children[1], c)
								{
									panic!(
										"{:?} != {:?} @ {:?}\n{:?}",
										interpret(&s.children[0].children[1], c),
										interpret(&s.children[1], c),
										s.value.pos,
										c
									);
								}
								Some(Unit)
							}
							"func" => Some(Function(
								s.children[0].children[1].value.clone(),
								s.children[1].clone(),
								c.clone(),
								None,
							)),
							"if_" => match interpret(&s.children[0].children[1], c) {
								Bool(true) => Some(Som(Box::new(interpret(&s.children[1], c)))),
								Bool(false) => Some(Non),
								a => panic!("{:?} {:?}\n{:?}", a, s, c),
							},
							"else_" => match interpret(&s.children[0].children[1], c) {
								Som(a) => Some(*a),
								Non => Some(interpret(&s.children[1], c)),
								a => panic!("{:?} {:?}\n{:?}", a, s, c),
							},
							"declare" => {
								let mut e = interpret(&s.children[1], c);
								if let Function(_, _, _, n) = &mut e {
									*n = Some(s.children[0].children[1].value.clone());
								}
								c.push_var(s.children[0].children[1].value.clone(), e);
								Some(Unit)
							},
							"define" => {
								let mut e = interpret(&s.children[1], c);
								if let Function(_, _, _, n) = &mut e {
									*n = Some(s.children[0].children[1].value.clone());
								}
								c.add_var(s.children[0].children[1].value.clone(), e);
								Some(Unit)
							},
							"try" => Some(Bool(destruct(
								&s.children[0].children[1],
								interpret(&s.children[1], c),
								c,
							))),
							"cons" => Some(Cons(
								Box::new(interpret(&s.children[0].children[1], c)),
								Box::new(interpret(&s.children[1], c)),
							)),
							"eq" => Some(Bool(
								interpret(&s.children[0].children[1], c)
									== interpret(&s.children[1], c),
							)),
							"gt" => get_num_bin_op(|a, b| Bool(a > b)),
							"lt" => get_num_bin_op(|a, b| Bool(a < b)),
							"add" => get_num_bin_op(|a, b| Num(a + b)),
							"sub" => get_num_bin_op(|a, b| Num(a - b)),
							"mul" => get_num_bin_op(|a, b| Num(a * b)),
							"div" => get_num_bin_op(|a, b| Num(a / b)),
							"mod" => get_num_bin_op(|a, b| Num(a % b)),
							"pow" => get_num_bin_op(|a, b| Num(a.powf(b))),
							_ => None,
						}
					} else {
						None
					} {
						e
					} else if let Function(x, y, mut z, n) = interpret(&s.children[0], c) {
						if let Some(n) = n {
							z.push_var(
								n.clone(),
								Function(x.clone(), y.clone(), z.clone(), Some(n)),
							);
						}
						z.push_var(x, interpret(&s.children[1], c));
						interpret(&y, &mut z)
					} else {
						panic!("{:?}\n{:?}", s, c);
					}
				}
				o => c.get_var(o).unwrap_or_else(|| panic!("unknown variable {} at {:?}\n{:?}", o, s.value.pos, c)),
			},
		}
	}

	interpret(s, &mut Env::new());
}
