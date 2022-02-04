#[derive(Clone)]
pub struct Token {
	pub string: String,
	pub row: usize,
	pub col: usize,
}

impl std::fmt::Debug for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{:?}@{}:{}", self.string, self.row, self.col)
	}
}

pub fn tokenize(file: &str) -> Vec<Token> {
	fn is_bracket(c: char) -> bool {
		crate::BRACKETS.iter().any(|&(a, b)| a == c || b == c)
	}
	fn char_type(c: char) -> u8 {
		((is_bracket(c) as u8) << 0)
			+ ((c.is_whitespace() as u8) << 1)
			+ ((c.is_numeric() as u8) << 2)
			+ (((c.is_alphabetic() || c == '_') as u8) << 3)
	}
	let mut row = 1;
	let mut col = 1;
	let mut tokens: Vec<Token> = vec![];
	for c in std::fs::read_to_string(file).unwrap().chars() {
		if tokens.is_empty()
			|| is_bracket(c)
			|| char_type(c) != char_type(tokens.last().unwrap().string.chars().next().unwrap())
		{
			tokens.push(Token { string: c.to_string(), row, col });
		} else {
			tokens.last_mut().unwrap().string.push(c);
		}
		if c == '\n' {
			row += 1;
			col = 1;
		} else {
			col += 1;
		}
	}
	tokens.into_iter().filter(|t| !t.string.chars().next().unwrap().is_whitespace()).collect()
}
