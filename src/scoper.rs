use crate::parser::AST;
use crate::tokenizer::Token;
use crate::tree::Tree;

#[derive(Clone, Debug, PartialEq)]
pub enum Lists {
	Curly,
	Paren,
}

#[derive(Clone)]
pub enum Leaf {
	Token(Token),
	Symbol(usize),
}

impl std::fmt::Debug for Leaf {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Leaf::Token(t) => write!(f, "t{:?}", t),
			Leaf::Symbol(s) => write!(f, "s{:?}", s),
		}
	}
}

pub type ScopedAST = Tree<Leaf, Lists, ()>;

pub fn scope(ast: &AST) -> Result<ScopedAST, String> {
	fn scope(ast: &AST, env: &mut Vec<String>) -> Result<ScopedAST, String> {
		match ast {
			AST::Leaf(token, ()) => Ok(ScopedAST::Leaf(
				match env.iter().enumerate().rev().find(|t| t.1 == &token.string) {
					Some((i, _)) => Leaf::Symbol(i),
					None => Leaf::Token(token.clone()),
				},
				(),
			)),
			AST::List(l, xs, ()) => Ok(ScopedAST::List(
				match &*l.string {
					"(" => Lists::Paren,
					"{" => Lists::Curly,
					s => Err(format!("unknown bracket {:?}", s))?,
				},
				xs.into_iter().map(|x| scope(x, env)).collect::<Result<Vec<_>, _>>()?,
				(),
			)),
			AST::Call(f, x, ()) => {
				if let AST::Leaf(Token { string: f, .. }, ()) = &**f {
					if let AST::Leaf(Token { string: x, .. }, ()) = &**x {
						if f == "func" {
							env.push(x.clone());
						}
					}
				}
				Ok(ScopedAST::call(scope(f, env)?, scope(x, env)?, ()))
			}
		}
	}
	scope(ast, &mut vec![])
}
