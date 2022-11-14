fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let mut tokens: Vec<(Token, usize, usize)> = Vec::new();
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'#' => {
				while i < chars.len() && chars[i] == '\n' {
					i += 1;
				}
			}
			' ' | '\t' | '\n' | '\r' => {}
			'(' => tokens.push((Token::Bracket(Bracket::Round, Side::Open), i, 1)),
			')' => tokens.push((Token::Bracket(Bracket::Round, Side::Close), i, 1)),
			'{' => tokens.push((Token::Bracket(Bracket::Curly, Side::Open), i, 1)),
			'}' => tokens.push((Token::Bracket(Bracket::Curly, Side::Close), i, 1)),
			'[' => tokens.push((Token::Bracket(Bracket::Square, Side::Open), i, 1)),
			']' => tokens.push((Token::Bracket(Bracket::Square, Side::Close), i, 1)),
			'"' => {
				let start = i;
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
				tokens.push((Token::String(string), start, i - start));
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

				tokens.push((Token::Identifier, start, i - start));
				i -= 1;
			}
		}
		i += 1;
	}

	for (token, idx, len) in &mut tokens {
		if *token == Token::Identifier {
			if '0' <= chars[*idx] && chars[*idx] <= '9' {
				if *len == 1 {
					*token = Token::Number(chars[*idx] as i64 - '0' as i64);
				} else {
					let mut value = 0;
					let (base, offset) = match (chars[*idx], chars[*idx + 1]) {
						('0', 'x') => (16, 2),
						('0', 'b') => (2, 2),
						(_, _) => (10, 0),
					};
					for i in *idx + offset..*idx + *len {
						if chars[i] != '_' {
							let digit = match chars[i] {
								'0'..='9' => chars[i] as i64 - '0' as i64,
								'A'..='F' => chars[i] as i64 - 'A' as i64 + 10,
								'a'..='f' => chars[i] as i64 - 'a' as i64 + 10,
								d => panic!("invalid digit {} in base {}", d, base),
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

	for (token, idx, len) in tokens {
		match token {
			Token::Bracket(_, _) => println!("b: {}", chars[idx]),
			Token::String(s) => println!("s: {}", s),
			Token::Identifier => {
				println!("i: {}", chars[idx..idx + len].iter().collect::<String>())
			}
			Token::Number(n) => println!("n: {}", n),
		}
	}
}

#[derive(Debug, PartialEq)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier,
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
