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
			// todo: numbers
			c => {
				let start = i;

				let is_splitter = |c: char| "\"#(){}{} \t\n".contains(c);
				let is_alphanumeric =
					|c: char| matches!(c, '_' | 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9');
				while i < chars.len()
					&& !is_splitter(chars[i])
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

	for (token, idx, len) in tokens {
		match token {
			Token::Bracket(_, _) => println!("b: {}", chars[idx]),
			Token::String(s) => println!("s: {}", s),
			Token::Identifier => {
				println!("i: {}", chars[idx..idx + len].iter().collect::<String>())
			}
		}
	}
}

#[derive(Debug)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier,
}

#[derive(Debug)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Debug)]
enum Side {
	Open,
	Close,
}
