use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::*;
use std::fs::read_to_string;
use std::rc::Rc;

#[derive(Clone)]
struct Token {
	string: String,
	t: TokenType,
	pos: Option<FilePosition>,
}

impl Token {
	fn new(s: &str) -> Self {
		Token::new_with_type(s, TokenType::Other)
	}

	fn new_with_type(s: &str, t: TokenType) -> Self {
		Token { string: s.to_string(), t, pos: None }
	}
}

impl Debug for Token {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}", self.string)
	}
}

#[derive(Clone, Debug, PartialEq)]
enum TokenType {
	Alphabetic,
	Whitespace,
	Numeric,
	Opener,
	Closer,
	Quote,
	Other,
}

#[derive(Clone)]
struct FilePosition {
	row: usize,
	col: usize,
	file: String,
}

impl Debug for FilePosition {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}:{}:{}", self.file, self.row, self.col)
	}
}

struct Ops {
	prefixes: HashMap<String, (String, u8, bool)>,
	infixes: HashMap<String, (String, u8, bool)>,
}

#[derive(Clone)]
struct S {
	value: Token,
	children: Vec<S>,
}

impl Debug for S {
	fn fmt(&self, f: &mut Formatter) -> Result {
		if self.children.is_empty() {
			write!(f, "{:?}", self.value)
		} else {
			write!(f, "({:?}", self.value)?;
			for s in &self.children {
				write!(f, " {:?}", s)?;
			}
			write!(f, ")")
		}
	}
}

struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
	ops: &'a Ops,
}

impl<'a> Context<'a> {
	fn get(&self) -> &Token {
		&self.tokens[self.index]
	}

	fn next(&mut self) {
		self.index += 1;
	}

	fn is_done(&self) -> bool {
		self.index >= self.tokens.len()
	}
}

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

impl Debug for Expr {
	fn fmt(&self, f: &mut Formatter) -> Result {
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

#[derive(Clone)]
struct Env {
	head: Option<Rc<RefCell<Node>>>,
}

struct Node {
	next: Option<Rc<RefCell<Node>>>,
	lhs: Token,
	rhs: Expr,
}

impl Env {
	fn get_var(&self, s: &str) -> Option<Expr> {
		self.head.as_ref().and_then(|n| n.borrow().get_var(s))
	}

	fn add_var(&mut self, lhs: Token, rhs: Expr) {
		if let Some(h) = &self.head {
			if h.borrow_mut().add_var(lhs.clone(), rhs.clone()) {
				self.push_var(lhs, rhs)
			}
		} else {
			self.push_var(lhs, rhs)
		}
	}

	fn push_var(&mut self, lhs: Token, rhs: Expr) {
		self.head = Some(Rc::new(RefCell::new(Node {
			next: self.head.clone(),
			lhs,
			rhs,
		})))
	}
}

impl Node {
	fn get_var(&self, s: &str) -> Option<Expr> {
		if self.lhs.string == s {
			Some(self.rhs.clone())
		} else {
			self.next.as_ref().and_then(|n| n.borrow().get_var(s))
		}
	}

	fn add_var(&mut self, lhs: Token, rhs: Expr) -> bool {
		if lhs.string == self.lhs.string {
			self.lhs = lhs;
			self.rhs = rhs;
			false
		} else if let Some(n) = &self.next {
			n.borrow_mut().add_var(lhs, rhs)
		} else {
			true
		}
	}
}

impl Debug for Env {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match &self.head {
			Some(n) => write!(f, "{:?}", n.borrow()),
			None => writeln!(f, "empty env"),
		}
	}
}

impl Debug for Node {
	fn fmt(&self, f: &mut Formatter) -> Result {
		if let Some(n) = &self.next {
			write!(f, "{:?}", n.borrow())?;
		}
		if let Some(p) = &self.lhs.pos {
			write!(f, "{:?} \t", p)?;
		}
		writeln!(f, "{:?}: {:?}", self.lhs, self.rhs)
	}
}

fn run_program_from_file(f: &str) {
	let mut ops = Ops { prefixes: HashMap::new(), infixes: HashMap::new() };
	let mut tokens = preprocess(&read_to_string(f).unwrap(), f.to_string(), &mut ops);
	fn preprocess(s: &str, file: String, ops: &mut Ops) -> Vec<Token> {
		let mut row = 1;
		let mut col = 1;
		s.chars()
			.map(|c| {
				let out = Token {
					string: c.to_string(),
					t: match c {
						c if c.is_alphabetic() || c == '_' => TokenType::Alphabetic,
						c if c.is_whitespace() => TokenType::Whitespace,
						c if c.is_numeric() => TokenType::Numeric,
						'(' | '{' | '[' => TokenType::Opener,
						')' | '}' | ']' => TokenType::Closer,
						'\"' => TokenType::Quote,
						_ => TokenType::Other,
					},
					pos: Some(FilePosition { row, col, file: file.clone() }),
				};
				if c == '\n' {
					row += 1;
					col = 1
				} else {
					col += 1;
				}
				out
			})
			.collect::<Vec<Token>>()
			.split(|t| t.string == "#")
			.enumerate()
			.flat_map(|(i, s)| {
				if i == 0 {
					return s.to_vec();
				}
				let i = s.iter().position(|t| t.string == "\n").unwrap_or(s.len());
				let (command, rest) = s.split_at(i);
				let c = command.iter().map(|t| &*t.string).collect::<String>();
				let c: Vec<&str> = c.split_whitespace().collect();
				match c.get(0) {
					Some(&"op") => {
						match c[4] {
							"prefix" => ops.prefixes.insert(
								c[1].to_string(),
								(c[2].to_string(), c[3].parse::<u8>().unwrap(), false),
							),
							"statement" => ops.prefixes.insert(
								c[1].to_string(),
								(c[2].to_string(), c[3].parse::<u8>().unwrap(), true),
							),
							"left" => ops.infixes.insert(
								c[1].to_string(),
								(c[2].to_string(), c[3].parse::<u8>().unwrap(), false),
							),
							"right" => ops.infixes.insert(
								c[1].to_string(),
								(c[2].to_string(), c[3].parse::<u8>().unwrap(), true),
							),
							c => panic!("{:?}", c),
						};
						rest.to_vec()
					}
					Some(&"include") => {
						let f = &c[1][1..c[1].len() - 1];
						let mut p = preprocess(&read_to_string(f).unwrap(), f.to_string(), ops);
						p.extend(rest.to_vec());
						p
					}
					_ => rest.to_vec(),
				}
			})
			.collect()
	}

	let mut in_string = false;
	tokens = tokens
		.into_iter()
		.fold(vec![], |mut acc: Vec<Token>, t| {
			use TokenType::*;
			match &*t.string {
				"\"" => {
					if in_string {
						in_string = false;
						acc.last_mut().unwrap().string.push('\"');
					} else {
						in_string = true;
						acc.push(t);
					}
				}
				_ => match acc.last_mut() {
					Some(s) if in_string => s.string.push_str(&t.string),
					Some(s) if s.t == t.t && s.t != Opener && s.t != Closer => {
						s.string.push_str(&t.string)
					}
					_ => acc.push(t),
				},
			}
			acc
		})
		.into_iter()
		.filter(|t| t.t != TokenType::Whitespace)
		.collect();

	tokens.insert(0, Token::new_with_type("{", TokenType::Opener));
	tokens.push(Token::new_with_type("}", TokenType::Closer));

	ops.prefixes.insert("print".to_string(), ("print".to_string(), 1, false));
	ops.prefixes.insert("assert".to_string(), ("assert".to_string(), 1, true));
	ops.infixes.insert("=".to_string(), ("set".to_string(), 2, true));
	ops.infixes.insert(":".to_string(), ("type".to_string(), 3, true));
	ops.infixes.insert("->".to_string(), ("func".to_string(), 4, true));
	ops.prefixes.insert("if".to_string(), ("if_".to_string(), 5, true));
	ops.infixes.insert("else".to_string(), ("else_".to_string(), 5, true));
	ops.infixes.insert("?=".to_string(), ("try".to_string(), 9, false));
	ops.infixes.insert("==".to_string(), ("eq".to_string(), 9, false));
	ops.infixes.insert(">".to_string(), ("gt".to_string(), 9, false));
	ops.infixes.insert("<".to_string(), ("lt".to_string(), 9, false));
	ops.infixes.insert("::".to_string(), ("cons".to_string(), 10, true));
	ops.infixes.insert("+".to_string(), ("add".to_string(), 11, false));
	ops.infixes.insert("-".to_string(), ("sub".to_string(), 11, false));
	ops.infixes.insert("*".to_string(), ("mul".to_string(), 12, false));
	ops.infixes.insert("/".to_string(), ("div".to_string(), 12, false));
	ops.infixes.insert("%".to_string(), ("mod".to_string(), 12, false));
	ops.infixes.insert("^".to_string(), ("pow".to_string(), 13, false));
	ops.infixes.insert("@".to_string(), ("call".to_string(), 16, false));

	fn parse(c: &mut Context, rbp: u8) -> S {
		fn build_arg_tree(func: &str, v: Vec<S>) -> S {
			v.into_iter().fold(S { value: Token::new(func), children: vec![] }, |acc, x| {
				S { value: Token::new("call"), children: vec![acc, x] }
			})
		}

		let op = c.get().clone();
		c.next();
		let mut lhs = match c.ops.prefixes.get(&op.string) {
			Some((f, bp, false)) => build_arg_tree(f, vec![parse(c, *bp)]),
			Some((f, bp, true)) => build_arg_tree(f, vec![parse(c, *bp), parse(c, *bp)]),
			None if op.t == TokenType::Opener => {
				let mut v = vec![];
				while c.get().t != TokenType::Closer {
					v.push(parse(c, 0));
				}
				c.next();
				S { value: op, children: v }
			}
			None => S { value: op, children: vec![] },
		};
		while !c.is_done() {
			match c.ops.infixes.get(&c.get().string) {
				Some((f, bp, a)) if bp > &rbp => {
					c.next();
					lhs = if f == "call" {
						S { value: Token::new(f), children: vec![lhs, parse(c, bp - *a as u8)] }
					} else {
						build_arg_tree(f, vec![lhs, parse(c, bp - *a as u8)])
					};
				}
				_ => break,
			}
		}
		lhs
	}

	interpret(
		&parse(&mut Context { index: 0, tokens: &tokens, ops: &ops }, 0),
		&mut Env { head: None },
	);

	fn interpret(s: &S, c: &mut Env) -> Expr {
		match s.value.t {
			TokenType::Numeric => return Num(s.value.string.parse().unwrap()),
			TokenType::Quote => return Str(s.value.clone()),
			_ => {}
		}

		match &*s.value.string {
			"(" => return interpret(&s.children[0], c),
			"{" => {
				let mut new_scope = c.clone();
				return s.children.iter().fold(Unit, |_, s| interpret(s, &mut new_scope));
			}
			"[" => {
				return s.children.iter().rev().fold(Empty, |acc, s| {
					Cons(Box::new(interpret(s, c)), Box::new(acc))
				})
			}
			"false" => return Bool(false),
			"true" => return Bool(true),
			"None" => return Non,
			"call" => {}
			o => {
				return c.get_var(o).unwrap_or_else(|| panic!("unknown variable {}\n{:?}", o, c))
			}
		}

		if s.children.len() == 2 {
			match s.children[0].value.string.as_str() {
				"Some" => return Som(Box::new(interpret(&s.children[1], c))),
				"print" => {
					println!("{:?}", interpret(&s.children[1], c));
					return Unit;
				}
				_ => {}
			}
		}

		if s.children.len() == 2 && s.children[0].children.len() == 2 {
			let get_a = |c: &mut Env| interpret(&s.children[0].children[1], c);
			let get_b = |c: &mut Env| interpret(&s.children[1], c);

			let mut get_num_bin_op = |f: fn(f64, f64) -> Expr| match (get_a(c), get_b(c)) {
				(Num(a), Num(b)) => f(a, b),
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
					return Unit;
				}
				"func" => {
					return Function(
						s.children[0].children[1].value.clone(),
						s.children[1].clone(),
						c.clone(),
						None,
					)
				}
				"if_" => match interpret(&s.children[0].children[1], c) {
					Bool(true) => return Som(Box::new(get_b(c))),
					Bool(false) => return Non,
					a => panic!("{:?} {:?}\n{:?}", a, s, c),
				},
				"else_" => match get_a(c) {
					Som(a) => return *a,
					Non => return get_b(c),
					a => panic!("{:?} {:?}\n{:?}", a, s, c),
				},
				"set" => {
					let mut e = get_b(c);
					if let Function(_, _, _, n) = &mut e {
						*n = Some(s.children[0].children[1].value.clone());
					}
					c.add_var(s.children[0].children[1].value.clone(), e);
					return Unit;
				}
				"try" => {
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
					return Bool(destruct(&s.children[0].children[1], get_b(c), c));
				}
				"cons" => return Cons(Box::new(get_a(c)), Box::new(get_b(c))),
				"eq" => return Bool(get_a(c) == get_b(c)),
				"gt" => return get_num_bin_op(|a, b| Bool(a > b)),
				"lt" => return get_num_bin_op(|a, b| Bool(a < b)),
				"add" => return get_num_bin_op(|a, b| Num(a + b)),
				"sub" => return get_num_bin_op(|a, b| Num(a - b)),
				"mul" => return get_num_bin_op(|a, b| Num(a * b)),
				"div" => return get_num_bin_op(|a, b| Num(a / b)),
				"mod" => return get_num_bin_op(|a, b| Num(a % b)),
				"pow" => return get_num_bin_op(|a, b| Num(a.powf(b))),
				_ => {}
			}
		}

		match interpret(&s.children[0], c) {
			Function(x, y, mut z, n) => {
				if let Some(n) = n {
					z.push_var(
						n.clone(),
						Function(x.clone(), y.clone(), z.clone(), Some(n)),
					);
				}
				z.push_var(x, interpret(&s.children[1], c));
				interpret(&y, &mut z)
			}
			o => panic!("{:?} {:?}\n{:?}", o, s, c),
		}
	}
}

fn main() {
	run_program_from_file("src/test.iz");
	run_program_from_file("src/scratch.iz");
}
