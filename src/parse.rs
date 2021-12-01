use crate::token::Token;
use crate::token::TokenType;

struct Context<'a> {
	index: usize,
	tokens: &'a [Token],
	ops: &'a crate::Ops,
}

impl<'a> Context<'a> {
	fn get(&self) -> &Token {
		&self.tokens[self.index]
	}
}

pub fn parse(tokens: &[Token], ops: &crate::Ops) -> crate::S {
	fn parse(c: &mut Context, rbp: u8) -> crate::S {
		fn build_arg_tree(func: &str, v: Vec<crate::S>) -> crate::S {
			v.into_iter().fold(
				crate::S { value: Token::new(func, TokenType::Other), children: vec![] },
				|acc, x| crate::S {
					value: Token::new("call", TokenType::Other),
					children: vec![acc, x],
				},
			)
		}

		let op = c.get().clone();
		c.index += 1;
		let mut lhs = match c.ops.prefixes.get(&op.string) {
			Some((f, bp, false)) => build_arg_tree(f, vec![parse(c, *bp)]),
			Some((f, bp, true)) => build_arg_tree(f, vec![parse(c, *bp), parse(c, *bp)]),
			None if op.t == TokenType::Opener => {
				let mut v = vec![];
				while c.get().t != TokenType::Closer {
					v.push(parse(c, 0));
				}
				c.index += 1;
				crate::S { value: op, children: v }
			}
			None => crate::S { value: op, children: vec![] },
		};
		while c.index < c.tokens.len() {
			match c.ops.infixes.get(&c.get().string) {
				Some((f, bp, a)) if bp > &rbp => {
					c.index += 1;
					lhs = if f == "call" {
						crate::S {
							value: Token::new(f, TokenType::Other),
							children: vec![lhs, parse(c, bp - *a as u8)],
						}
					} else {
						build_arg_tree(f, vec![lhs, parse(c, bp - *a as u8)])
					};
				}
				_ => break,
			}
		}
		lhs
	}

	parse(&mut Context { index: 0, tokens, ops }, 0)
}
