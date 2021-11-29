use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::*;
use std::fs::read_to_string;
use std::rc::Rc;

#[derive(Clone)]
struct Position {
	row: usize,
	col: usize,
	file: String,
}

impl Debug for Position {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}:{}:{}", self.file, self.row, self.col)
	}
}

#[derive(Clone)]
struct Token(String, TokenType, Option<Position>);

#[derive(Clone, PartialEq)]
enum TokenType {
	Alphabetic,
	Whitespace,
	Numeric,
	Opener,
	Closer,
	Strin,
	Other,
}

impl Debug for Token {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}", self.0)
	}
}

struct Ops {
	prefixes: HashMap<String, (String, bool, u8, bool)>,
	infixes: HashMap<String, (String, bool, u8, bool)>,
}

// HERE
fn preprocess(s: &str, file: String) -> (Vec<Token>, Ops) {
	let mut position = Position { row: 1, col: 1, file };
	let mut program = Vec::with_capacity(s.len());
	for c in s.chars() {
		let t = match c {
			c if c.is_alphabetic() || c == '_' => TokenType::Alphabetic,
			c if c.is_whitespace() => TokenType::Whitespace,
			c if c.is_numeric() => TokenType::Numeric,
			'(' | '{' | '[' => TokenType::Opener,
			')' | '}' | ']' => TokenType::Closer,
			'\"' => TokenType::Strin,
			_ => TokenType::Other,
		};
		program.push(Token(c.to_string(), t, Some(position.clone())));
		if c == '\n' {
			position.row += 1;
			position.col = 1
		} else {
			position.col += 1;
		}
	}

	let mut operators = Ops { prefixes: HashMap::new(), infixes: HashMap::new() };
	let program = program
		.split(|t| t.0 == "#")
		.enumerate()
		.map(|(i, s)| {
			if i == 0 {
				return s.to_vec();
			}
			let i = s.iter().position(|t| t.0 == "\n").unwrap_or(s.len());
			let (command, rest) = s.split_at(i);
			let c = command.iter().map(|t| &*t.0).collect::<String>();
			let c: Vec<&str> = c.split_whitespace().collect();
			match c.get(0) {
				Some(&"op") => {
					match c[4] {
						"prefix" => operators.prefixes.insert(
							c[1].to_string(),
							(c[2].to_string(), false, c[3].parse::<u8>().unwrap(), false),
						),
						"statement" => operators.prefixes.insert(
							c[1].to_string(),
							(c[2].to_string(), false, c[3].parse::<u8>().unwrap(), true),
						),
						"left" => operators.infixes.insert(
							c[1].to_string(),
							(c[2].to_string(), false, c[3].parse::<u8>().unwrap(), false),
						),
						"right" => operators.infixes.insert(
							c[1].to_string(),
							(c[2].to_string(), false, c[3].parse::<u8>().unwrap(), true),
						),
						c => panic!("{:?}", c),
					};
					rest.to_vec()
				}
				Some(&"include") => {
					let f = &c[1][1..c[1].len() - 1];
					let (mut p, ops) =
						preprocess(&read_to_string(f).unwrap(), f.to_string());
					operators.prefixes.extend(ops.prefixes);
					operators.infixes.extend(ops.infixes);
					p.extend(rest.to_vec());
					p
				}
				_ => rest.to_vec(),
			}
		})
		.flatten()
		.collect();
	(program, operators)
}

#[derive(Clone)]
struct S(Token, Vec<S>);

impl Debug for S {
	fn fmt(&self, f: &mut Formatter) -> Result {
		if self.1.is_empty() {
			write!(f, "{:?}", self.0)
		} else {
			write!(f, "({:?}", self.0)?;
			for s in &self.1 {
				write!(f, " {:?}", s)?;
			}
			write!(f, ")")
		}
	}
}

struct Context<'a>(usize, &'a [Token], &'a Ops);

impl<'a> Context<'a> {
	fn get(&self) -> &Token {
		&self.1[self.0]
	}

	fn next(&mut self) {
		self.0 += 1;
	}
}

fn parse(mut p: (Vec<Token>, Ops)) -> S {
	p.1.prefixes.insert("print".to_string(), ("print".to_string(), true, 1, false));
	p.1.prefixes.insert("assert".to_string(), ("assert".to_string(), true, 1, true));
	p.1.infixes.insert("=".to_string(), ("set".to_string(), true, 2, true));
	p.1.infixes.insert(":".to_string(), ("type".to_string(), true, 3, true));
	p.1.infixes.insert("->".to_string(), ("func".to_string(), true, 4, true));
	p.1.prefixes.insert("if".to_string(), ("if_".to_string(), true, 5, true));
	p.1.infixes.insert("else".to_string(), ("else_".to_string(), true, 5, true));
	p.1.infixes.insert("?=".to_string(), ("try".to_string(), true, 9, false));
	p.1.infixes.insert("==".to_string(), ("eq".to_string(), true, 9, false));
	p.1.infixes.insert(">".to_string(), ("gt".to_string(), true, 9, false));
	p.1.infixes.insert("<".to_string(), ("lt".to_string(), true, 9, false));
	p.1.infixes.insert("::".to_string(), ("cons".to_string(), true, 10, true));
	p.1.infixes.insert("+".to_string(), ("add".to_string(), true, 11, false));
	p.1.infixes.insert("-".to_string(), ("sub".to_string(), true, 11, false));
	p.1.infixes.insert("*".to_string(), ("mul".to_string(), true, 12, false));
	p.1.infixes.insert("/".to_string(), ("div".to_string(), true, 12, false));
	p.1.infixes.insert("%".to_string(), ("mod".to_string(), true, 12, false));
	p.1.infixes.insert("^".to_string(), ("pow".to_string(), true, 13, false));
	p.1.infixes.insert("@".to_string(), ("call".to_string(), true, 16, false));

	let mut tokens: Vec<Token> =
		p.0.into_iter()
			.fold(
				(false, vec![]),
				|(mut in_s, mut acc): (bool, Vec<Token>), t| {
					match &*t.0 {
						"\"" => match in_s {
							true => {
								in_s = false;
								acc.last_mut().unwrap().0.push('\"');
							}
							false => {
								in_s = true;
								acc.push(Token("\"".to_string(), t.1, t.2));
							}
						},
						_ => match acc.last_mut() {
							Some(s) if in_s => s.0.push_str(&t.0),
							Some(s) if s.1 == t.1 && s.1 != TokenType::Opener && s.1 != TokenType::Closer => {
								s.0.push_str(&t.0)
							}
							_ => acc.push(t),
						},
					}
					(in_s, acc)
				},
			)
			.1
			.into_iter()
			.filter(|t| t.1 != TokenType::Whitespace)
			.collect();
	tokens.insert(
		0,
		Token(
			"{".to_string(),
			TokenType::Opener,
			None,
		),
	);
	tokens.push(Token(
		"}".to_string(),
		TokenType::Closer,
		None,
	));

	fn get_lhs(func: &str, p: Option<Position>, i: bool, v: Vec<S>) -> S {
		let f = Token(func.to_string(), TokenType::Other, p.clone());
		match i {
			true => S(f, v),
			false => v.into_iter().fold(S(f, vec![]), |acc, x| {
				S(Token("call".to_string(), TokenType::Other, p.clone()), vec![acc, x])
			}),
		}
	}

	fn expr(c: &mut Context, rbp: u8) -> S {
		let mut op = c.get().clone();
		c.next();
		let mut lhs = match c.2.prefixes.get(&op.0) {
			Some((f, i, bp, false)) => get_lhs(f, op.2, *i, vec![expr(c, *bp)]),
			Some((f, i, bp, true)) => get_lhs(f, op.2, *i, vec![expr(c, *bp), expr(c, *bp)]),
			None if op.1 == TokenType::Opener => {
				let mut v = vec![];
				while c.get().1 != TokenType::Closer {
					v.push(expr(c, 0));
				}
				c.next();
				S(op, v)
			}
			None => S(op, vec![]),
		};
		while c.0 < c.1.len() {
			op = c.get().clone();
			match c.2.infixes.get(&op.0) {
				Some((f, i, bp, true)) if bp > &rbp => {
					c.next();
					lhs = get_lhs(f, op.2, *i, vec![lhs, expr(c, bp - 1)]);
				}
				Some((f, i, bp, false)) if bp > &rbp => {
					c.next();
					lhs = get_lhs(f, op.2, *i, vec![lhs, expr(c, *bp)]);
				}
				_ => break,
			}
		}
		lhs
	}

	expr(&mut Context(0, &tokens, &p.1), 0)
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
	Function(Token, S, Env, Option<String>),
	Str(Token),
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
			(Str(a), Str(b)) => a.0 == b.0,
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
			Function(x, y, _, _) => write!(f, "{:?}->{:?}", x, y),
			Str(s) => write!(f, "{}", s.0),
		}
	}
}

#[derive(Clone)]
struct Env(Option<Rc<RefCell<Node>>>);
struct Node(Option<Rc<RefCell<Node>>>, Frame);

#[derive(Clone)]
struct Frame(String, Expr, Option<Position>);

impl Env {
	fn new() -> Env {
		Env(None)
	}

	fn get_var(&self, s: &str) -> Option<Expr> {
		self.0.as_ref().and_then(|n| n.borrow().get_var(s))
	}

	fn add_var(&mut self, f: Frame) {
		if let None = self.0 {
			self.push_var(f);
		} else if let Err(f) = self.0.as_ref().map(|n| n.borrow_mut().add_var(f)).unwrap() {
			self.push_var(f);
		}
	}

	fn push_var(&mut self, f: Frame) {
		self.0 = Some(Rc::new(RefCell::new(Node(self.0.clone(), f))))
	}
}

impl Node {
	fn get_var(&self, s: &str) -> Option<Expr> {
		if self.1 .0 == s {
			Some(self.1 .1.clone())
		} else {
			self.0.as_ref().and_then(|n| n.borrow().get_var(s))
		}
	}

	fn add_var(&mut self, f: Frame) -> std::result::Result<(), Frame> {
		if f.0 == self.1 .0 {
			self.1 = f;
			Ok(())
		} else {
			match &self.0 {
				Some(n) => n.borrow_mut().add_var(f),
				None => Err(f),
			}
		}
	}
}

impl Debug for Env {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match &self.0 {
			Some(n) => write!(f, "{:?}", n.borrow()),
			None => writeln!(f, "empty env"),
		}
	}
}

impl Debug for Node {
	fn fmt(&self, f: &mut Formatter) -> Result {
		if let Some(n) = &self.0 {
			write!(f, "{:?}", n.borrow())?;
		}
		writeln!(f, "{:?} \t{}: {:?}", self.1 .2, self.1 .0, self.1 .1)
	}
}

fn interpret(s: &S, c: &mut Env) -> Expr {
	let get_arg = |c: &mut Env, i: usize| interpret(&s.1[i], &mut c.clone());
	let mut get_num_bin_op = |f: fn(f64, f64) -> f64| match (get_arg(c, 0), get_arg(c, 1)) {
		(Num(a), Num(b)) => Num(f(a, b)),
		(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
	};

	match &*s.0 .0 {
		"(" => get_arg(c, 0),
		"{" => {
			let mut new_scope = c.clone();
			s.1.iter().fold(Unit, |_, s| interpret(s, &mut new_scope))
		}
		"[" => s.1.iter().rev().fold(Empty, |acc, s| {
			Cons(Box::new(interpret(s, &mut c.clone())), Box::new(acc))
		}),
		"assert" => {
			if get_arg(c, 0) != get_arg(c, 1) {
				panic!(
					"{:?} != {:?} @ {:?}\n{:?}",
					get_arg(c, 0),
					get_arg(c, 1),
					s.0 .2,
					c
				);
			}
			Unit
		}
		"print" => {
			println!("{:?}", get_arg(c, 0));
			Unit
		}
		"set" => {
			let mut e = get_arg(c, 1);
			if let Function(_, _, _, n) = &mut e {
				*n = Some(s.1[0].0 .0.clone());
			}
			c.add_var(Frame(s.1[0].0 .0.clone(), e, s.0 .2.clone()));
			Unit
		}
		"func" => Function(s.1[0].0.clone(), s.1[1].clone(), c.clone(), None),
		"if_" => match interpret(&s.1[0], c) {
			Bool(true) => Som(Box::new(get_arg(c, 1))),
			Bool(false) => Non,
			a => panic!("{:?} {:?}\n{:?}", a, s, c),
		},
		"else_" => match get_arg(c, 0) {
			Som(a) => *a,
			Non => get_arg(c, 1),
			a => panic!("{:?} {:?}\n{:?}", a, s, c),
		},
		"try" => {
			fn destruct(s: &S, a: Expr, c: &mut Env) -> bool {
				match s.0 .0.as_str() {
					"cons" => match a {
						Cons(a, b) => destruct(&s.1[0], *a, c) && destruct(&s.1[1], *b, c),
						_ => false,
					},
					"call" if s.1[0].0 .0 == "Some" => match a {
						Som(a) => destruct(&s.1[1], *a, c),
						_ => false,
					},
					"_" => true,
					_ => {
						c.push_var(Frame(s.0 .0.clone(), a, s.0 .2.clone()));
						true
					}
				}
			}
			Bool(destruct(&s.1[0], get_arg(c, 1), c))
		}
		"eq" => Bool(get_arg(c, 0) == get_arg(c, 1)),
		"gt" => match (get_arg(c, 0), get_arg(c, 1)) {
			(Num(a), Num(b)) => Bool(a > b),
			(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
		},
		"lt" => match (get_arg(c, 0), get_arg(c, 1)) {
			(Num(a), Num(b)) => Bool(a < b),
			(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
		},
		"cons" => Cons(Box::new(get_arg(c, 0)), Box::new(get_arg(c, 1))),
		"add" => get_num_bin_op(|a, b| a + b),
		"sub" => get_num_bin_op(|a, b| a - b),
		"mul" => get_num_bin_op(|a, b| a * b),
		"div" => get_num_bin_op(|a, b| a / b),
		"mod" => get_num_bin_op(|a, b| a % b),
		"pow" => get_num_bin_op(|a, b| a.powf(b)),
		"false" => Bool(false),
		"true" => Bool(true),
		"None" => Non,
		"call" if s.1[0].0 .0 == "Some" => Som(Box::new(get_arg(c, 1))),
		"call" => match get_arg(c, 0) {
			Function(x, y, mut z, n) => {
				if let Some(n) = n {
					z.push_var(Frame(
						n.clone(),
						Function(x.clone(), y.clone(), z.clone(), Some(n)),
						s.0 .2.clone(),
					));
				}
				z.push_var(Frame(x.0, get_arg(c, 1), s.0 .2.clone()));
				interpret(&y, &mut z)
			}
			o => panic!("{:?} {:?}\n{:?}", o, s, c),
		},
		o => match s.0 .1 {
			TokenType::Numeric => Num(s.0 .0.parse().unwrap()),
			TokenType::Strin => Str(s.0.clone()),
			_ => c.get_var(o).unwrap_or_else(|| panic!("{:?} {:?}\n{:?}", o, s, c)),
		},
	}
}

fn run_program_from_file(f: &str) {
	interpret(
		&parse(preprocess(&read_to_string(f).unwrap(), f.to_string())),
		&mut Env::new(),
	);
}

fn main() {
	run_program_from_file("src/test.iz");
	run_program_from_file("src/scratch.iz");
}
