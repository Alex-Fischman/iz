mod expr;
mod parse;
mod token;

#[derive(Clone)]
pub struct S {
	pub value: token::Token,
	pub children: Vec<S>,
}

impl std::fmt::Debug for S {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		if self.children.is_empty() {
			write!(f, "{}", self.value.string)
		} else {
			write!(f, "({}", self.value.string)?;
			for s in &self.children {
				write!(f, " {:?}", s)?;
			}
			write!(f, ")")
		}
	}
}

use std::collections::HashMap;
pub struct Ops {
	pub prefixes: HashMap<String, (String, u8, bool)>,
	pub infixes: HashMap<String, (String, u8, bool)>,
}

fn run_program_from_file(f: &str) {
	let mut ops = Ops { prefixes: HashMap::new(), infixes: HashMap::new() };
	let mut tokens = preprocess(
		&std::fs::read_to_string(f).unwrap(),
		f.to_string(),
		&mut ops,
	);
	fn preprocess(s: &str, file: String, ops: &mut Ops) -> Vec<token::Token> {
		token::add_positions_to_characters(s, file)
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
						let mut p =
							preprocess(&std::fs::read_to_string(f).unwrap(), f.to_string(), ops);
						p.extend(rest.to_vec());
						p
					}
					_ => rest.to_vec(),
				}
			})
			.collect()
	}

	tokens = token::merge_matching_token_types(tokens);
	tokens.insert(0, token::Token::new("{", token::TokenType::Opener));
	tokens.push(token::Token::new("}", token::TokenType::Closer));

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

	expr::interpret(&parse::parse(&tokens, &ops));
}

fn main() {
	let args: Vec<String> = std::env::args().collect();
	if args.len() > 1 {
		run_program_from_file(&args[1]);
	} else {
		run_program_from_file("examples/test.iz");
		run_program_from_file("examples/scratch.iz");
	}
}
