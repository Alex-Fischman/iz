#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Location<'a>(pub usize, pub usize, pub &'a [char]);

impl std::fmt::Debug for Location<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let fold = |(row, col), &c| match c {
			'\n' => (row + 1, 1),
			_ => (row, col + 1),
		};
		let (row0, col0) = self.2[..self.0].iter().fold((1, 1), fold);
		let (row1, col1) = self.to_chars().iter().fold((row0, col0), fold);
		write!(f, "{}:{}-{}:{}", row0, col0, row1, col1)
	}
}

impl<'a> Location<'a> {
	pub fn to_chars(self) -> &'a [char] {
		&self.2[self.0..self.0 + self.1]
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<'a> {
	Ident(Location<'a>),
	String(String, Location<'a>),
	Number(i64, Location<'a>),
	Opener(Bracket, Location<'a>),
	Closer(Bracket, Location<'a>),
}

pub fn tokenize(chars: &[char]) -> Result<Vec<Token>, String> {
	fn search<F: Fn(&char) -> bool>(chars: &[char], start: usize, f: F) -> usize {
		chars[start..].iter().position(f).unwrap_or(chars.len() - start)
	}
	let mut t = vec![];
	let mut i = 0;
	while let Some(c) = chars.get(i) {
		match c {
			'"' => {
				i += 1;
				let idx = i;
				let mut s = "".to_owned();
				loop {
					match chars.get(i) {
						Some('"') => break,
						Some('\\') => {
							i += 1;
							match chars.get(i) {
								Some('\\') => s.push('\\'),
								Some('"') => s.push('\"'),
								Some('n') => s.push('\n'),
								Some('t') => s.push('\t'),
								_ => Err(format!(
									"no valid escape character at {:?}",
									Location(i, 0, chars)
								))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(format!("no end quote at {:?}", Location(i, 0, chars)))?,
					}
					i += 1;
				}
				t.push(Token::String(s, Location(idx, i - idx, chars)));
			}
			'#' => i += search(chars, i, |c| *c == '\n'),
			'(' => t.push(Token::Opener(Bracket::Round, Location(i, 1, chars))),
			')' => t.push(Token::Closer(Bracket::Round, Location(i, 1, chars))),
			'{' => t.push(Token::Opener(Bracket::Curly, Location(i, 1, chars))),
			'}' => t.push(Token::Closer(Bracket::Curly, Location(i, 1, chars))),
			'[' => t.push(Token::Opener(Bracket::Square, Location(i, 1, chars))),
			']' => t.push(Token::Closer(Bracket::Square, Location(i, 1, chars))),
			' ' | '\t' | '\n' => {}
			c => {
				let is_alphanumeric = |c: char| c.is_ascii_alphanumeric() || c == '_';
				let is_splitter = |c: char| "\"#(){}{} \t\n".chars().any(|a| a == c);
				let l = Location(
					i,
					if is_alphanumeric(*c) {
						search(chars, i, |c: &char| !is_alphanumeric(*c))
					} else {
						search(chars, i, |c: &char| is_splitter(*c) || is_alphanumeric(*c))
					},
					chars,
				);
				t.push(
					match l.to_chars().iter().rev().try_fold((0, 1, false), |(a, b, z), c| {
						match c {
							'0' => Some((a, b * 10, true)),
							'1' => Some((a + b, b * 10, true)),
							'2' => Some((a + b * 2, b * 10, true)),
							'3' => Some((a + b * 3, b * 10, true)),
							'4' => Some((a + b * 4, b * 10, true)),
							'5' => Some((a + b * 5, b * 10, true)),
							'6' => Some((a + b * 6, b * 10, true)),
							'7' => Some((a + b * 7, b * 10, true)),
							'8' => Some((a + b * 8, b * 10, true)),
							'9' => Some((a + b * 9, b * 10, true)),
							'_' => Some((a, b, z)),
							_ => None,
						}
					}) {
						Some((n, _, z)) if z => Token::Number(n, l),
						_ => Token::Ident(l),
					},
				);
				i += l.1 - 1;
			}
		}
		i += 1;
	}
	Ok(t)
}

#[test]
fn tokenize_test() {
	assert_eq!(
		tokenize(&"\"\"\"".chars().collect::<Vec<char>>()),
		Err("no end quote at 1:4-1:4".to_owned())
	);
	assert_eq!(
		tokenize(&"\"\\".chars().collect::<Vec<char>>()),
		Err("no valid escape character at 1:3-1:3".to_owned())
	);
	assert_eq!(
		tokenize(&"\"\\a".chars().collect::<Vec<char>>()),
		Err("no valid escape character at 1:3-1:3".to_owned())
	);
	let chars = "214s2135**ad_fe2 _-_".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![
			Token::Ident(Location(0, 8, &chars)),
			Token::Ident(Location(8, 2, &chars)),
			Token::Ident(Location(10, 6, &chars)),
			Token::Ident(Location(17, 1, &chars)),
			Token::Ident(Location(18, 1, &chars)),
			Token::Ident(Location(19, 1, &chars)),
		]
	);
	let chars = "\"\"".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![Token::String("".to_owned(), Location(1, 0, &chars))]
	);
	let chars = "5_84_39".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars).unwrap(), vec![Token::Number(58439, Location(0, 7, &chars))]);
	let chars = ")".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![Token::Closer(Bracket::Round, Location(0, 1, &chars))]
	);
	let chars = "a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = 1_000_000 (c = {a})"
		.chars()
		.collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![
			Token::Ident(Location(0, 1, &chars)),
			Token::Ident(Location(2, 1, &chars)),
			Token::String(
				"text with  s, \ts, \ns, \"s, and \\s".to_owned(),
				Location(5, 35, &chars)
			),
			Token::Ident(Location(42, 1, &chars)),
			Token::Ident(Location(44, 1, &chars)),
			Token::Number(1000000, Location(46, 9, &chars)),
			Token::Opener(Bracket::Round, Location(56, 1, &chars)),
			Token::Ident(Location(57, 1, &chars)),
			Token::Ident(Location(59, 1, &chars)),
			Token::Opener(Bracket::Curly, Location(61, 1, &chars)),
			Token::Ident(Location(62, 1, &chars)),
			Token::Closer(Bracket::Curly, Location(63, 1, &chars)),
			Token::Closer(Bracket::Round, Location(64, 1, &chars)),
		]
	);
}
