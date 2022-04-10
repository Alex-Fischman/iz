use crate::tokenizer::Token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lists {
	Group,
	Block,
}

pub const BRACKETS: [(char, (char, Lists)); 2] =
	[('(', (')', Lists::Group)), ('{', ('}', Lists::Block))];
pub const PREFIXES: [(&str, (&str, u8)); 1] = [("-", ("neg", 9))];
// last arg is 0 for left assoc, 1 for right assoc
pub const INFIXES: [(&str, (&str, u8, u8)); 3] =
	[("+", ("add", 7, 0)), ("-", ("sub", 7, 0)), ("*", ("mul", 8, 0))];

#[derive(PartialEq)]
pub enum AST {
	Leaf(Token),
	List(Lists, Vec<AST>),
}

impl std::fmt::Debug for AST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			AST::Leaf(t) => write!(f, "{:?}", t),
			AST::List(l, v) => write!(f, "{:?}{:#?}", l, v),
		}
	}
}

use std::collections::HashMap;
struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
	brackets: &'a HashMap<char, (char, Lists)>,
	prefixes: &'a HashMap<&'a str, (&'a str, u8)>,
	infixes: &'a HashMap<&'a str, (&'a str, u8, u8)>,
}

pub fn parse(tokens: &[Token]) -> AST {
	fn parse(c: &mut Context, rbp: u8) -> AST {
		let t = c.tokens[c.index].clone();
		c.index += 1;
		let mut lhs = if let Some(&(s, bp)) = c.prefixes.get(&*t.string) {
			let op = AST::Leaf(Token { string: s.to_string(), ..c.tokens[c.index - 1] });
			AST::List(Lists::Block, vec![parse(c, bp), op])
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
					let op = AST::Leaf(Token { string: s.to_string(), ..c.tokens[c.index - 1] });
					lhs = AST::List(Lists::Block, vec![lhs, parse(c, bp - assoc), op]);
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
	AST::List(Lists::Block, v)
}
