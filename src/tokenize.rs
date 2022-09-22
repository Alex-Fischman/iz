use crate::{Error, Location};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
	Ident(String, Location),
	String(String, Location),
	Number(i64, Location),
	Opener(Bracket, Location),
	Closer(Bracket, Location),
}

pub fn tokenize(chars: &[char]) -> Result<Vec<Token>, Error> {
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
								_ => Err(Error(
									"missing valid escape character".to_owned(),
									Location(i, 0),
								))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(Error("no end quote".to_owned(), Location(i, 0)))?,
					}
					i += 1;
				}
				t.push(Token::String(s, Location(idx, i - idx)));
			}
			'#' => i += search(chars, i, |c| *c == '\n'),
			'(' => t.push(Token::Opener(Bracket::Round, Location(i, 1))),
			')' => t.push(Token::Closer(Bracket::Round, Location(i, 1))),
			'{' => t.push(Token::Opener(Bracket::Curly, Location(i, 1))),
			'}' => t.push(Token::Closer(Bracket::Curly, Location(i, 1))),
			'[' => t.push(Token::Opener(Bracket::Square, Location(i, 1))),
			']' => t.push(Token::Closer(Bracket::Square, Location(i, 1))),
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
				);
				let cs = &chars[l.0..l.0 + l.1];
				t.push(
					match cs.iter().rev().try_fold((0, 1, false), |(a, b, z), c| match c {
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
					}) {
						Some((n, _, z)) if z => Token::Number(n, l),
						_ => Token::Ident(cs.iter().collect(), l),
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
		Err(Error("no end quote".to_owned(), Location(3, 0)))
	);
	assert_eq!(
		tokenize(&"\"\\".chars().collect::<Vec<char>>()),
		Err(Error("missing valid escape character".to_owned(), Location(2, 0)))
	);
	assert_eq!(
		tokenize(&"\"\\a".chars().collect::<Vec<char>>()),
		Err(Error("missing valid escape character".to_owned(), Location(2, 0)))
	);
	assert_eq!(
		tokenize(&"214s2135**ad_fe2 _-_".chars().collect::<Vec<char>>()).unwrap(),
		vec![
			Token::Ident("214s2135".to_owned(), Location(0, 8)),
			Token::Ident("**".to_owned(), Location(8, 2)),
			Token::Ident("ad_fe2".to_owned(), Location(10, 6)),
			Token::Ident("_".to_owned(), Location(17, 1)),
			Token::Ident("-".to_owned(), Location(18, 1)),
			Token::Ident("_".to_owned(), Location(19, 1)),
		]
	);
	assert_eq!(
		tokenize(&"\"\"".chars().collect::<Vec<char>>()).unwrap(),
		vec![Token::String("".to_owned(), Location(1, 0))]
	);
	assert_eq!(
		tokenize(&"5_84_39".chars().collect::<Vec<char>>()).unwrap(),
		vec![Token::Number(58439, Location(0, 7))]
	);
	assert_eq!(
		tokenize(&")".chars().collect::<Vec<char>>()).unwrap(),
		vec![Token::Closer(Bracket::Round, Location(0, 1))]
	);
	assert_eq!(
		tokenize(
			&"a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = 1_000_000 (c = {a})"
				.chars()
				.collect::<Vec<char>>()
		)
		.unwrap(),
		vec![
			Token::Ident("a".to_owned(), Location(0, 1)),
			Token::Ident("=".to_owned(), Location(2, 1)),
			Token::String("text with  s, \ts, \ns, \"s, and \\s".to_owned(), Location(5, 35)),
			Token::Ident("b".to_owned(), Location(42, 1)),
			Token::Ident("=".to_owned(), Location(44, 1)),
			Token::Number(1000000, Location(46, 9)),
			Token::Opener(Bracket::Round, Location(56, 1)),
			Token::Ident("c".to_owned(), Location(57, 1)),
			Token::Ident("=".to_owned(), Location(59, 1)),
			Token::Opener(Bracket::Curly, Location(61, 1)),
			Token::Ident("a".to_owned(), Location(62, 1)),
			Token::Closer(Bracket::Curly, Location(63, 1)),
			Token::Closer(Bracket::Round, Location(64, 1)),
		]
	);
}
