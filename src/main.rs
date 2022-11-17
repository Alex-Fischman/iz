fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let (tokens, identifiers) = tokenizer(&chars);
	let trees = parser(&tokens, &identifiers);

	fn print_trees(trees: &[Tree], identifiers: &[String], depth: usize) {
		for tree in trees {
			print!("{}", "\t".repeat(depth));
			match tree {
				Tree::Brackets(b, cs) => {
					println!("b: {}", b.to_char(Side::Open));
					print_trees(cs, identifiers, depth + 1)
				}
				Tree::String(s) => println!("s: {}", s),
				Tree::Identifier(i) => println!("i: {}", identifiers[*i]),
				Tree::Number(n) => println!("n: {}", n),
			}
		}
	}
	print_trees(&trees, &identifiers, 0);
}

fn tokenizer(chars: &[char]) -> (Vec<Token>, Vec<String>) {
	let mut tokens: Vec<Token> = Vec::new();
	let mut identifiers: Vec<String> = Vec::new();
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'#' => {
				while i < chars.len() && chars[i] != '\n' {
					i += 1;
				}
			}
			' ' | '\t' | '\n' | '\r' => {}
			'(' => tokens.push(Token::Bracket(Bracket::Round, Side::Open)),
			')' => tokens.push(Token::Bracket(Bracket::Round, Side::Close)),
			'{' => tokens.push(Token::Bracket(Bracket::Curly, Side::Open)),
			'}' => tokens.push(Token::Bracket(Bracket::Curly, Side::Close)),
			'[' => tokens.push(Token::Bracket(Bracket::Square, Side::Open)),
			']' => tokens.push(Token::Bracket(Bracket::Square, Side::Close)),
			'"' => {
				i += 1;
				let mut string = String::new();
				loop {
					if i >= chars.len() {
						panic!("missing end quote");
					} else if chars[i] == '\\' {
						i += 1;
						string.push(match chars.get(i) {
							Some('\\') => '\\',
							Some('t') => '\t',
							Some('n') => '\n',
							Some('r') => '\r',
							Some('"') => '"',
							Some(c) => panic!("invalid escape character {}", c),
							None => panic!("missing escape character"),
						});
					} else if chars[i] == '"' {
						break;
					} else {
						string.push(chars[i]);
					}
					i += 1;
				}
				tokens.push(Token::String(string));
			}
			c => {
				let start = i;
				let is_alphanumeric =
					|c: char| matches!(c, '_' | 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9');
				while i < chars.len()
					&& !"\"#(){}{} \t\n".contains(chars[i])
					&& is_alphanumeric(c) == is_alphanumeric(chars[i])
				{
					i += 1;
				}

				let s: String = chars[start..i].iter().collect();
				if let Some(i) = identifiers.iter().position(|i| *i == s) {
					tokens.push(Token::Identifier(i));
				} else {
					identifiers.push(s);
					tokens.push(Token::Identifier(identifiers.len() - 1));
				}

				i -= 1;
			}
		}

		i += 1;
	}

	for token in &mut tokens {
		if let Token::Identifier(i) = token {
			let chars: Vec<char> = identifiers[*i].chars().collect();
			if '0' <= chars[0] && chars[0] <= '9' {
				if chars.len() == 1 {
					*token = Token::Number(chars[0] as i64 - '0' as i64);
				} else {
					let mut value = 0;
					let (base, start) = match (chars[0], chars[1]) {
						('0', 'x') => (16, 2),
						('0', 'b') => (2, 2),
						(_, _) => (10, 0),
					};
					if start >= chars.len() {
						panic!("no digits after base specifier");
					}
					for c in chars.iter().skip(start) {
						if *c != '_' {
							let digit = match *c {
								'0'..='9' => *c as i64 - '0' as i64,
								'A'..='F' => *c as i64 - 'A' as i64 + 10,
								'a'..='f' => *c as i64 - 'a' as i64 + 10,
								d => panic!("invalid digit {}", d),
							};
							if digit >= base {
								panic!("invalid digit {} in base {}", digit, base);
							}
							value = value * base + digit;
						}
					}
					*token = Token::Number(value);
				}
			}
		}
	}

	(tokens, identifiers)
}

fn parser(tokens: &[Token], identifiers: &[String]) -> Vec<Tree> {
	fn parse(
		tokens: &[Token],
		_identifiers: &[String],
		i: &mut usize,
		mut searching: Option<Bracket>,
	) -> Vec<Tree> {
		let mut trees = Vec::new();
		while *i < tokens.len() {
			let token = &tokens[*i];
			*i += 1;
			trees.push(match token {
				Token::Bracket(b, Side::Open) => {
					Tree::Brackets(*b, parse(tokens, _identifiers, i, Some(*b)))
				}
				Token::Bracket(b, Side::Close) => match (b, searching) {
					(b, Some(c)) if *b == c => {
						searching = None;
						break;
					}
					(b, _) => panic!("extra {}", b.to_char(Side::Close)),
				},
				Token::String(s) => Tree::String(s.clone()),
				Token::Identifier(i) => Tree::Identifier(*i),
				Token::Number(n) => Tree::Number(*n),
			});
		}
		if let Some(b) = searching {
			panic!("extra {}", b.to_char(Side::Open));
		}

		trees
	}

	parse(tokens, identifiers, &mut 0, None)
}

#[derive(Debug, PartialEq)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier(usize),
	Number(i64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Side {
	Open,
	Close,
}

impl Bracket {
	fn to_char(self, side: Side) -> char {
		match (self, side) {
			(Bracket::Round, Side::Open) => '(',
			(Bracket::Round, Side::Close) => ')',
			(Bracket::Curly, Side::Open) => '{',
			(Bracket::Curly, Side::Close) => '}',
			(Bracket::Square, Side::Open) => '[',
			(Bracket::Square, Side::Close) => ']',
		}
	}
}

#[derive(Debug, PartialEq)]
enum Tree {
	Brackets(Bracket, Vec<Tree>),
	String(String),
	Identifier(usize),
	Number(i64),
}
