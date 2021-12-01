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

use TokenType::*;
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
	s.chars()
		.scan(FilePosition { row: 1, col: 1, file }, |f, c| {
			let out = Token {
				string: c.to_string(),
				t: match c {
					c if c.is_alphabetic() || c == '_' => Alphabetic,
					c if c.is_whitespace() => Whitespace,
					c if c.is_numeric() => Numeric,
					'(' | '{' | '[' => Opener,
					')' | '}' | ']' => Closer,
					'\"' => Quote,
					_ => Other,
				},
				pos: Some(f.clone()),
			};
			if c == '\n' {
				f.row += 1;
				f.col = 1
			} else {
				f.col += 1;
			}
			Some(out)
		})
		.collect()
}

pub fn merge_matching_token_types(v: Vec<Token>) -> Vec<Token> {
	v.split(|t| t.t == Quote)
		.scan(false, |i, ts| {
			*i = !*i;
			Some(if *i {
				ts.to_vec()
			} else {
				vec![Token {
					string: ts.iter().map(|t| t.string.clone()).collect(),
					t: Quote,
					pos: ts[0].pos.clone(),
				}]
			})
		})
		.flatten()
		.fold(vec![], |mut out: Vec<Token>, t| {
			match out.last_mut() {
				Some(s) if s.t == t.t && s.t != Opener && s.t != Closer && s.t != Quote => {
					s.string.push_str(&t.string)
				}
				_ => out.push(t),
			}
			out
		})
		.into_iter()
		.filter(|t| t.t != Whitespace)
		.collect()
}
