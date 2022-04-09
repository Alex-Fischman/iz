use crate::tokenizer::Token;

#[derive(Clone, Copy)]
pub enum Assoc {
	Left = 0,
	Right = 1,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lists {
	Curly,
	Round,
	Square,
}

pub const BRACKETS: [(char, (char, Lists)); 3] =
	[('(', (')', Lists::Round)), ('{', ('}', Lists::Curly)), ('[', (']', Lists::Square))];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg", 9))];
pub const INFIXES: [(&str, (&str, u8, Assoc)); 6] = [
	("=", ("set", 3, Assoc::Right)),
	("==", ("eql", 6, Assoc::Left)),
	("+", ("add", 7, Assoc::Left)),
	("-", ("sub", 7, Assoc::Left)),
	("*", ("mul", 8, Assoc::Left)),
	("@", ("call", 15, Assoc::Left)),
];

#[derive(PartialEq)]
pub enum AST {
	Leaf(Token),
	List(Lists, Vec<AST>),
	Call(Box<AST>, Vec<AST>),
}

impl std::fmt::Debug for AST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			AST::Leaf(t) => write!(f, "{:?}", t),
			AST::List(l, v) => write!(f, "{:?}{:#?}", l, v),
			AST::Call(a, bs) => write!(f, "{:?} {:#?}", a, bs),
		}
	}
}

use std::collections::HashMap;
struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
	brackets: &'a HashMap<char, (char, Lists)>,
	prefixes: &'a HashMap<&'a str, (&'a str, u8)>,
	infixes: &'a HashMap<&'a str, (&'a str, u8, Assoc)>,
}

pub fn parse(tokens: &[Token]) -> AST {
	fn parse(c: &mut Context, rbp: u8) -> AST {
		let get_op = |c: &Context, s: &str| {
			Box::new(AST::Leaf(Token { string: s.to_string(), ..c.tokens[c.index - 1] }))
		};

		let t = c.tokens[c.index].clone();
		c.index += 1;

		let mut lhs = if let Some(&(s, bp)) = c.prefixes.get(&*t.string) {
			AST::Call(get_op(c, s), vec![parse(c, bp)])
		} else if let Some(&(end, l)) = c.brackets.get(&t.string.chars().next().unwrap()) {
			let mut v = vec![];
			while c.tokens[c.index].string != end.to_string() {
				v.push(parse(c, 0));
			}
			c.index += 1;
			AST::List(l, v)
		} else {
			AST::Leaf(t)
		};

		while c.index < c.tokens.len() {
			match c.infixes.get(&*c.tokens[c.index].string) {
				Some(&(s, bp, assoc)) if bp > rbp => {
					c.index += 1;
					lhs = AST::Call(get_op(c, s), vec![lhs, parse(c, bp - assoc as u8)]);
				}
				_ => break,
			}
		}

		lhs
	}

	let mut c = Context {
		tokens,
		index: 0,
		brackets: &HashMap::from(BRACKETS),
		prefixes: &HashMap::from(PREFIXES),
		infixes: &HashMap::from(INFIXES),
	};

	let mut v = vec![];
	while c.index < c.tokens.len() {
		v.push(parse(&mut c, 0));
	}
	AST::List(Lists::Curly, v)
}
