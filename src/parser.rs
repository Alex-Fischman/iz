use crate::tokenizer::Token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Lists {
	Block,
	Group,
}

pub const BRACKETS: [(char, (char, Lists)); 2] =
	[('(', (')', Lists::Group)), ('{', ('}', Lists::Block))];

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
}

pub fn parse(tokens: &[Token]) -> AST {
	fn parse(c: &mut Context) -> AST {
		let t = c.tokens[c.index].clone();
		c.index += 1;
		if let Some(&(end, l)) = c.brackets.get(&t.string.chars().next().unwrap()) {
			let mut v = vec![];
			while c.tokens[c.index].string != end.to_string() {
				v.push(parse(c));
			}
			c.index += 1;
			AST::List(l, v)
		} else {
			AST::Leaf(t)
		}
	}
	let mut c = Context {
		tokens,
		index: 0,
		brackets: &HashMap::from(BRACKETS),
	};
	let mut v = vec![];
	while c.index < c.tokens.len() {
		v.push(parse(&mut c));
	}
	AST::List(Lists::Group, v)
}
