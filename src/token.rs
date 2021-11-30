#[derive(Clone)]
pub struct Token {
	pub string: String,
	pub t: TokenType,
	pub pos: Option<FilePosition>,
}

impl Token {
	pub fn new(s: &str, t: TokenType) -> Token {
		Token { string: s.to_string(), t, pos: None }
	}
}

impl std::fmt::Debug for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.string)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
	Alphabetic,
	Whitespace,
	Numeric,
	Opener,
	Closer,
	Quote,
	Other,
}

impl TokenType {
	fn is_mergeable(&self) -> bool {
		use TokenType::*;
		match self {
			Alphabetic | Whitespace | Numeric | Other => true,
			Opener | Closer | Quote => false,
		}
	}
}

#[derive(Clone)]
pub struct FilePosition {
	row: usize,
	col: usize,
	file: String,
}

impl std::fmt::Debug for FilePosition {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}:{}:{}", self.file, self.row, self.col)
	}
}

pub fn add_positions_to_characters(s: &str, file: String) -> Vec<Token> {
	let mut row = 1;
	let mut col = 1;
	s.chars()
		.map(|c| {
			let out = Token {
				string: c.to_string(),
				t: match c {
					c if c.is_alphabetic() || c == '_' => TokenType::Alphabetic,
					c if c.is_whitespace() => TokenType::Whitespace,
					c if c.is_numeric() => TokenType::Numeric,
					'(' | '{' | '[' => TokenType::Opener,
					')' | '}' | ']' => TokenType::Closer,
					'\"' => TokenType::Quote,
					_ => TokenType::Other,
				},
				pos: Some(FilePosition { row, col, file: file.clone() }),
			};
			if c == '\n' {
				row += 1;
				col = 1
			} else {
				col += 1;
			}
			out
		})
		.collect()
}

pub fn merge_matching_token_types(v: Vec<Token>) -> Vec<Token> {
	let mut out: Vec<Token> = vec![];
	v.split(|t| t.t == TokenType::Quote)
		.enumerate()
		.flat_map(|(i, ts)| {
			if i % 2 == 0 {
				ts.to_vec()
			} else {
				vec![Token {
					string: ts.iter().map(|t| t.string.clone()).collect(),
					t: TokenType::Quote,
					pos: ts[0].pos.clone(),
				}]
			}
		})
		.for_each(|t| match out.last_mut() {
			Some(s) if s.t == t.t && s.t.is_mergeable() => s.string.push_str(&t.string),
			_ => out.push(t),
		});
	out.into_iter().filter(|t| t.t != TokenType::Whitespace).collect()
}
