use crate::token::Token;
use crate::token::TokenType;

#[derive(Clone)]
pub struct S {
	pub value: Token,
	pub children: Vec<S>,
}

impl std::fmt::Debug for S {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

pub struct Ops {
	pub prefixes: std::collections::HashMap<String, (String, u8, bool)>,
	pub infixes: std::collections::HashMap<String, (String, u8, bool)>,
}

impl Ops {
	pub fn new() -> Ops {
		use std::collections::HashMap;
		Ops { prefixes: HashMap::new(), infixes: HashMap::new() }
	}
}

pub struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
}

impl<'a> Context<'a> {
	pub fn new(tokens: &'a [Token]) -> Context<'a> {
		Context { index: 0, tokens }
	}

	pub fn get(&self) -> &Token {
		&self.tokens[self.index]
	}

	pub fn next(&mut self) {
		self.index += 1;
	}

	pub fn is_done(&self) -> bool {
		self.index >= self.tokens.len()
	}
}

pub fn parse(c: &mut Context, ops: &Ops, rbp: u8) -> S {
	fn build_arg_tree(func: &str, v: Vec<S>) -> S {
		v.into_iter().fold(
			S { value: Token::new(func, TokenType::Other), children: vec![] },
			|acc, x| S { value: Token::new("call", TokenType::Other), children: vec![acc, x] },
		)
	}

	let op = c.get().clone();
	c.next();
	let mut lhs = match ops.prefixes.get(&op.string) {
		Some((f, bp, false)) => build_arg_tree(f, vec![parse(c, ops, *bp)]),
		Some((f, bp, true)) => build_arg_tree(f, vec![parse(c, ops, *bp), parse(c, ops, *bp)]),
		None if op.t == TokenType::Opener => {
			let mut v = vec![];
			while c.get().t != TokenType::Closer {
				v.push(parse(c, ops, 0));
			}
			c.next();
			S { value: op, children: v }
		}
		None => S { value: op, children: vec![] },
	};
	while !c.is_done() {
		match ops.infixes.get(&c.get().string) {
			Some((f, bp, a)) if bp > &rbp => {
				c.next();
				lhs = if f == "call" {
					S {
						value: Token::new(f, TokenType::Other),
						children: vec![lhs, parse(c, ops, bp - *a as u8)],
					}
				} else {
					build_arg_tree(f, vec![lhs, parse(c, ops, bp - *a as u8)])
				};
			}
			_ => break,
		}
	}
	lhs
}
