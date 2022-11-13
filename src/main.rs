fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let mut tokens = Vec::new();
	let is_alphabetic = |c| match c {
		'a'..='z' | 'A'..='A' | '_' => true,
		_ => false,
	};
	let is_numeric = |c| match c {
		'0'..='9' => true,
		_ => false,
	};
	let is_whitespace = |c| match c {
		' ' | '\t' | '\n' | '\r' => true,
		_ => false,
	};

	let mut i = 0;
	while i < chars.len() {
		let (typ, len) =
			match chars[i] {
				'#' => {
					let mut len = 1;
					while let Some(c) = chars.get(i + len) {
						if *c == '\n' {
							break;
						}
						len += 1;
					}
					(TokenType::Whitespace, 1)
				}
				'(' | ')' | '{' | '}' | '[' | ']' => (TokenType::Bracket, 1),
				c if c.is_whitespace() => (TokenType::Whitespace, 1),
				'"' => {
					let mut escaped = false;
					let mut len = 1;
					loop {
						let c = *chars.get(i + len).expect("missing end quote");
						if escaped {
							escaped = false;
						} else if c == '\\' {
							escaped = true;
						} else if c == '"' {
							break;
						}
						len += 1;
					}
					(TokenType::String, len)
				}
				c if c.is_numeric() || c == '-' => {
					let mut len = 1;
					while let Some(c) = chars.get(i + len) {
						if *c != '-'
							&& *c != '1' && *c != '2' && *c != '3'
							&& *c != '4' && *c != '5' && *c != '6'
							&& *c != '7' && *c != '8' && *c != '9'
							&& *c != '0' && *c != '_'
						{
							break;
						}
						len += 1;
					}
					(TokenType::Integer, len)
				}
				c => {
					let letters = is_alphabetic(c);
					let mut len = 1;
					while let Some(c) = chars.get(i + len) {
						if letters {
							if !(is_alphabetic(*c) || is_numeric(*c)) || is_whitespace(*c) {
								break;
							}
						} else {
							if is_alphabetic(*c) || is_numeric(*c) || is_whitespace(*c) {
								break;
							}
						}
						len += 1;
					}
					(TokenType::Identifier, len)
				}
			};
		tokens.push(Token(Location::new(i, len), typ));
		i += len;
	}

	for token in tokens {
		println!(
			"{:?} | {:?}",
			chars[token.0.idx..token.0.idx + token.0.len].iter().collect::<String>(),
			token.1,
		);
	}
}

#[derive(Debug)]
struct Location {
	idx: usize,
	len: usize,
}

impl Location {
	fn new(idx: usize, len: usize) -> Self {
		Location { idx, len }
	}
}

#[derive(Debug)]
struct Token(Location, TokenType);
#[derive(Debug)]
enum TokenType {
	Bracket,
	Whitespace,
	String,
	Integer,
	Identifier,
}
