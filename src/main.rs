fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let (tokens, identifiers) = tokenizer(&chars);

	for token in tokens {
		match token {
			Token::Bracket(b, s) => println!(
				"b: {}",
				match (b, s) {
					(Bracket::Round, Side::Open) => '(',
					(Bracket::Round, Side::Close) => ')',
					(Bracket::Curly, Side::Open) => '{',
					(Bracket::Curly, Side::Close) => '}',
					(Bracket::Square, Side::Open) => '[',
					(Bracket::Square, Side::Close) => ']',
				}
			),
			Token::String(s) => println!("s: {}", s),
			Token::Identifier(i) => println!("i: {}", identifiers[i]),
			Token::Number(n) => println!("n: {}", n),
		}
	}
}

fn tokenizer(chars: &[char]) -> (Vec<Token>, Vec<String>) {
	let mut identifiers: Vec<String> = Vec::new();
	let mut identifier = |start, end| {
		let s: String = chars[start..end].iter().collect();
		for (i, identifier) in identifiers.iter().enumerate() {
			if identifier == &s {
				return Token::Identifier(i);
			}
		}
		identifiers.push(s);
		Token::Identifier(identifiers.len() - 1)
	};

	let mut tokens: Vec<Token> = Vec::new();
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
				tokens.push(identifier(start, i));
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
					for i in start..chars.len() {
						if chars[i] != '_' {
							let digit = match chars[i] {
								'0'..='9' => chars[i] as i64 - '0' as i64,
								'A'..='F' => chars[i] as i64 - 'A' as i64 + 10,
								'a'..='f' => chars[i] as i64 - 'a' as i64 + 10,
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

	return (tokens, identifiers);
}

#[derive(Debug, PartialEq)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier(usize),
	Number(i64),
}

#[derive(Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Debug, PartialEq)]
enum Side {
	Open,
	Close,
}
