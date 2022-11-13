fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let mut tokens: Vec<Token> = Vec::new();
	let mut i = 0;
	while i < chars.len() {
		tokens.push(Token(Location::new(i, 1), TokenType::Unknown));
		i += 1;
	}

	for token in tokens {
		match token.1 {
			TokenType::Unknown => println!(
				"b: {:?}",
				chars[token.0.idx..token.0.idx + token.0.len].iter().collect::<String>()
			),
		}
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
#[derive(Debug, PartialEq)]
enum TokenType {
	Unknown,
}
