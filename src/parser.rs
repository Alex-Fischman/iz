use crate::tokenizer::Token;

#[derive(Debug)]
pub enum AST {
	Token(Token),
	Call(Box<AST>, Box<AST>),
	List(Token, Vec<AST>, Token),
}

use std::collections::HashMap;
struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
	brackets: &'a HashMap<char, char>,
	prefixes: &'a HashMap<&'a str, (&'a str, u8)>,
	statements: &'a HashMap<&'a str, (&'a str, u8)>,
	infixes: &'a HashMap<&'a str, (&'a str, u8, crate::Assoc)>,
}

pub fn parse(tokens: &[Token]) -> AST {
	fn call(a: AST, b: AST) -> AST {
		AST::Call(Box::new(a), Box::new(b))
	}
	fn parse(c: &mut Context, rbp: u8) -> AST {
		fn get_op(c: &Context, s: &str) -> AST {
			AST::Token(Token { string: s.to_string(), ..c.tokens[c.index - 1] })
		}

		let t = c.tokens[c.index].clone();
		c.index += 1;
		let mut lhs = if let Some(&(s, bp)) = c.prefixes.get(&*t.string) {
			call(get_op(c, s), parse(c, bp))
		} else if let Some(&(s, bp)) = c.statements.get(&*t.string) {
			call(call(get_op(c, s), parse(c, bp)), parse(c, bp))
		} else if let Some(end) = c.brackets.get(&t.string.chars().next().unwrap()) {
			let mut v = vec![];
			while c.tokens[c.index].string != end.to_string() {
				v.push(parse(c, 0));
			}
			c.index += 1;
			AST::List(t, v, c.tokens[c.index - 1].clone())
		} else {
			AST::Token(t)
		};

		while c.index < c.tokens.len() {
			match c.infixes.get(&*c.tokens[c.index].string) {
				Some(&(s, bp, assoc)) if bp > rbp => {
					c.index += 1;
					if s == "@" {
						lhs = call(lhs, parse(c, bp - assoc as u8));
					} else {
						lhs = call(call(get_op(c, s), lhs), parse(c, bp - assoc as u8));
					}
				}
				_ => break,
			}
		}

		lhs
	}

	let mut c = Context {
		tokens,
		index: 0,
		brackets: &HashMap::from(crate::BRACKETS),
		prefixes: &HashMap::from(crate::PREFIXES),
		statements: &HashMap::from(crate::STATEMENTS),
		infixes: &HashMap::from(crate::INFIXES),
	};
	parse(&mut c, 0)
}
