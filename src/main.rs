use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::*;
use std::rc::Rc;

fn main() {
	let parse_test = |a, b| {
		let (p, mut ops) = preprocess(a, "parse test".to_string());
		add_std_ops(&mut ops);
		assert_eq!(
			format!("{:?}", parse(tokenize(p), ops)),
			format!("({{ {})", b)
		)
	};
	parse_test("1", "1");
	parse_test("a + b", "(add a b)");
	parse_test("if a b", "(if_ a b)");
	parse_test("a + b + c", "(add (add a b) c)");
	parse_test("a = b = c", "(set a (set b c))");
	parse_test("1 + f = g = h * 3", "(set (add 1 f) (set g (mul h 3)))");
	parse_test("a + b * c * d + e", "(add (add a (mul (mul b c) d)) e)");
	parse_test("#op . dot 15 left\na . b", "(call (call dot a) b)");
	parse_test("#op - neg 14 prefix\n-a + b", "(add (call neg a) b)");
	parse_test(
		"#op - neg 14 prefix\n- -1 * 2",
		"(mul (call neg (call neg 1)) 2)",
	);
	parse_test(
		"#op - neg 14 prefix\n- -f + g",
		"(add (call neg (call neg f)) g)",
	);
	parse_test("(0(0))", "(( 0 (( 0))");
	parse_test("((0)(0))", "(( (( 0) (( 0))");
	parse_test("(((0)))", "(( (( (( 0)))");
	parse_test(
		"if a b else if c d else e",
		"(else_ (if_ a b) (else_ (if_ c d) e))",
	);
	parse_test(
		"a = if 0 b else c = d",
		"(set a (set (else_ (if_ 0 b) c) d))",
	);
	parse_test("  \" asdfs sadfasdf \"  ", "\" asdfs sadfasdf \"");
	parse_test("\"asdf\" # \"comment\"", "\"asdf\"");

	run_program("src/test.iz");
	run_program("src/scratch.iz");
}

fn run_program(f: &str) {
	let (p, mut ops) = preprocess(&std::fs::read_to_string(f).unwrap(), f.to_string());
	add_std_ops(&mut ops);
	interpret(&parse(tokenize(p), ops), &mut Env::new());
}

#[derive(Clone)]
struct Position {
	row: usize,
	col: usize,
	file: String,
}

impl Position {
	fn new(row: usize, col: usize, file: String) -> Position {
		Position {
			row: row,
			col: col,
			file: file,
		}
	}

	fn empty() -> Position {
		Position::new(0, 0, "".to_string())
	}
}

impl Debug for Position {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}:{}:{}", self.file, self.row, self.col)
	}
}

type Ops = HashMap<(String, bool), (String, bool, u8, OpType)>;

use OpType::*;
#[derive(Debug)]
enum OpType {
	Prefix,
	Prefix2,
	Left,
	Right,
}

impl OpType {
	fn is_prefix(&self) -> bool {
		match self {
			Prefix => true,
			Prefix2 => true,
			Left => false,
			Right => false,
		}
	}
}

fn add_std_ops(ops: &mut Ops) {
	let mut new_op = |n: &str, f: &str, bp: u8, ty: OpType| {
		ops.insert(
			(n.to_string(), ty.is_prefix()),
			(f.to_string(), true, bp, ty),
		)
	};
	new_op("print", "print", 1, Prefix);
	new_op("assert", "assert", 1, Prefix2);
	new_op("=", "set", 2, Right);
	new_op(":", "type", 3, Right);
	new_op("->", "func", 4, Right);
	new_op("if", "if_", 5, Prefix2);
	new_op("else", "else_", 5, Right);
	new_op("?=", "try", 9, Left);
	new_op("==", "eq", 9, Left);
	new_op(">", "gt", 9, Left);
	new_op("<", "lt", 9, Left);
	new_op("::", "cons", 10, Right);
	new_op("+", "add", 11, Left);
	new_op("-", "sub", 11, Left);
	new_op("*", "mul", 12, Left);
	new_op("/", "div", 12, Left);
	new_op("%", "mod", 12, Left);
	new_op("^", "pow", 13, Left);
	new_op("@", "call", 16, Left);
}

#[derive(Clone)]
struct Token(String, TokenType, Position);

use TokenType::*;
#[derive(Clone, Debug, PartialEq)]
enum TokenType {
	Alphabetic,
	Whitespace,
	Numeric,
	Opener,
	Closer,
	Strin,
	Other,
}

impl PartialEq for Token {
	fn eq(&self, t: &Token) -> bool {
		self.0 == t.0 && self.1 == t.1
	}
}

impl Debug for Token {
	fn fmt(&self, f: &mut Formatter) -> Result {
		write!(f, "{}", self.0)
	}
}

fn preprocess(s: &str, file: String) -> (Vec<Token>, Ops) {
	let mut position = Position::new(1, 1, file);
	let mut program = Vec::with_capacity(s.len());
	for c in s.chars() {
		let t = match c {
			c if c.is_alphabetic() || c == '_' => Alphabetic,
			c if c.is_whitespace() => Whitespace,
			c if c.is_numeric() => Numeric,
			'(' | '{' | '[' => Opener,
			')' | '}' | ']' => Closer,
			'\"' => Strin,
			_ => Other,
		};
		program.push(Token(c.to_string(), t, position.clone()));
		if c == '\n' {
			position.row += 1;
			position.col = 1
		} else {
			position.col += 1;
		}
	}
	let mut operators = HashMap::new();
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
					let t = match c[4] {
						"prefix" => Prefix,
						"statement" => Prefix2,
						"left" => Left,
						"right" => Right,
						c => panic!("{:?}", c),
					};
					operators.insert(
						(c[1].to_string(), t.is_prefix()),
						(c[2].to_string(), false, c[3].parse::<u8>().unwrap(), t),
					);
					rest.to_vec()
				}
				Some(&"include") => {
					let f = &c[1][1..c[1].len() - 1];
					let (mut p, ops) =
						preprocess(&std::fs::read_to_string(f).unwrap(), f.to_string());
					operators.extend(ops);
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

fn tokenize(s: Vec<Token>) -> Vec<Token> {
	s.into_iter()
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
						Some(s) if s.1 == t.1 && s.1 != Opener && s.1 != Closer => {
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
		.filter(|t| t.1 != Whitespace)
		.collect()
}

#[derive(Clone, PartialEq)]
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

fn parse(mut tokens: Vec<Token>, operators: Ops) -> S {
	fn get_lhs(func: &str, p: Position, i: bool, v: Vec<S>) -> S {
		let f = Token(func.to_string(), Other, p.clone());
		match i {
			true => S(f, v),
			false => v.into_iter().fold(S(f, vec![]), |acc, x| {
				S(Token("call".to_string(), Other, p.clone()), vec![acc, x])
			}),
		}
	}

	fn expr(c: &mut Context, rbp: u8) -> S {
		let mut op = c.get().clone();
		c.next();
		let mut lhs = match c.2.get(&(op.0.clone(), true)) {
			Some((f, i, bp, Prefix)) => get_lhs(f, op.2, *i, vec![expr(c, *bp)]),
			Some((f, i, bp, Prefix2)) => get_lhs(f, op.2, *i, vec![expr(c, *bp), expr(c, *bp)]),
			Some(_) => unreachable!(),
			None if op.1 == Opener => {
				let mut v = vec![];
				while c.get().1 != Closer {
					v.push(expr(c, 0));
				}
				c.next();
				S(op, v)
			}
			None => S(op, vec![]),
		};
		while c.0 < c.1.len() {
			op = c.get().clone();
			match c.2.get(&(op.0, false)) {
				Some((f, i, bp, Right)) if bp > &rbp => {
					c.next();
					lhs = get_lhs(f, op.2, *i, vec![lhs, expr(c, bp - 1)]);
				}
				Some((f, i, bp, Left)) if bp > &rbp => {
					c.next();
					lhs = get_lhs(f, op.2, *i, vec![lhs, expr(c, *bp)]);
				}
				_ => break,
			}
		}
		lhs
	}

	tokens.insert(0, Token("{".to_string(), Opener, Position::empty()));
	tokens.push(Token("}".to_string(), Closer, Position::empty()));
	expr(&mut Context(0, &tokens, &operators), 0)
}

use Expr::*;
#[derive(Clone)]
enum Expr {
	Unit,
	Var(Token),
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
			(Var(a), Var(b)) => a == b,
			(Bool(a), Bool(b)) => a == b,
			(Num(a), Num(b)) => a == b,
			(Som(a), Som(b)) => a == b,
			(Non, Non) => true,
			(Cons(a, b), Cons(c, d)) => a == c && b == d,
			(Empty, Empty) => true,
			(Str(a), Str(b)) => a == b,
			_ => false,
		}
	}
}

impl Debug for Expr {
	fn fmt(&self, f: &mut Formatter) -> Result {
		match self {
			Unit => write!(f, "{{}}"),
			Var(s) => write!(f, "v{:?}", s),
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
struct Node(Frame, Option<Rc<RefCell<Node>>>);

#[derive(Clone)]
struct Frame(String, Expr, Position);

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
		self.0 = Some(Rc::new(RefCell::new(Node(f, self.0.clone()))))
	}
}

impl Node {
	fn get_var(&self, s: &str) -> Option<Expr> {
		if self.0 .0 == s {
			Some(self.0 .1.clone())
		} else {
			self.1.as_ref().and_then(|n| n.borrow().get_var(s))
		}
	}

	fn add_var(&mut self, f: Frame) -> std::result::Result<(), Frame> {
		if f.0 == self.0 .0 {
			self.0 = f;
			Ok(())
		} else {
			match &self.1 {
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
		if let Some(n) = &self.1 {
			write!(f, "{:?}", n.borrow())?;
		}
		writeln!(f, "{:?} \t{}: {:?}", self.0 .2, self.0 .0, self.0 .1)
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
		"set" => match (&interpret(&s.1[0], &mut Env::new()), get_arg(c, 1)) {
			(Var(v), mut e) => {
				if let Function(_, _, _, n) = &mut e {
					*n = Some(v.0.clone());
				}
				c.add_var(Frame(v.0.clone(), e, s.0 .2.clone()));
				Unit
			}
			(a, b) => panic!("{:?} {:?} {:?}\n{:?}", a, b, s, c),
		},
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
			fn destruct(e: &Expr, a: Expr, c: &mut Env) -> bool {
				match (e, a) {
					(Var(s), a) => {
						if s.0 != "_" {
							c.push_var(Frame(s.0.clone(), a, s.2.clone()));
						}
						true
					}
					(Cons(e, f), Cons(a, b)) => destruct(e, *a, c) && destruct(f, *b, c),
					_ => false,
				}
			}
			Bool(destruct(
				&interpret(&s.1[0], &mut Env::new()),
				get_arg(c, 1),
				c,
			))
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
		"call" => match get_arg(c, 0) {
			Var(Token(t, _, _)) if t == "Some" => Som(Box::new(get_arg(c, 1))),
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
		"false" => Bool(false),
		"true" => Bool(true),
		"None" => Non,
		o => match s.0 .1 {
			Numeric => Num(s.0.0.parse().unwrap()),
			Strin => Str(s.0.clone()),
			_ => match c.get_var(o) {
				Some(e) => e,
				None => Var(s.0.clone()),
			},
		},
	}
}
