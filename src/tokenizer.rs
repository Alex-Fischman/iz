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

pub fn tokenize(s: &str) -> Vec<Token> {
	fn is_bracket(c: char) -> bool {
		crate::BRACKETS.iter().any(|&(a, b)| a == c || b == c)
	}
	fn char_type(c: char) -> u8 {
		(((c.is_alphabetic() || c.is_numeric() || c == '_') as u8) << 0)
			+ ((c.is_whitespace() as u8) << 1)
			+ ((is_bracket(c) as u8) << 2)
	}
	let mut row = 1;
	let mut col = 1;
	let mut tokens: Vec<Token> = vec![];
	let mut in_comment = false;
	let mut in_string = false;
	tokens.push(Token { string: "{".to_string(), row: 0, col: 0 });
	for c in s.chars() {
		if in_comment {
			if c == '\n' {
				in_comment = false;
			}
		} else if c == '#' {
			in_comment = true;
		} else if in_string {
			tokens.last_mut().unwrap().string.push(c);
			if c == '"' {
				in_string = false;
			}
		} else if c == '"' {
			in_string = true;
			tokens.push(Token { string: c.to_string(), row, col });
		} else if tokens.is_empty()
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
	tokens.push(Token { string: "}".to_string(), row: 0, col: 0 });
	tokens.into_iter().filter(|t| !t.string.chars().next().unwrap().is_whitespace()).collect()
}
