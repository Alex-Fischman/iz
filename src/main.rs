#[derive(PartialEq, Debug)]
enum Error {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
	MissingEndQuote(Location),
	InvalidEscapeCharacter(Location),
}

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
	let text =
		std::fs::read_to_string(file).map_err(|_| Error::CouldNotReadFile(file.to_owned()))?;
	let chars: Vec<char> = text.chars().collect();

	let o = tokenize(&chars)?;

	println!();
	for token in o.tokens {
		println!("{}", token.to_string(file, &chars, &o.idents, &o.strings));
	}
	println!();
	Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Location {
	idx: usize,
	len: usize,
}

impl Location {
	fn new(idx: usize, len: usize) -> Location {
		Location { idx, len }
	}

	fn to_string(self, file: &str, chars: &[char]) -> String {
		let (row, col) = chars[..self.idx].iter().fold((1, 1), |(row, col), c| match c {
			'\n' => (row + 1, 1),
			_ => (row, col + 1),
		});
		format!("{}:{}:{}", file, row, col)
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum TokenData {
	Identifier(usize),
	String(usize),
	Number(i64),
	OpenBracket(Bracket),
	CloseBracket(Bracket),
}

#[derive(Debug, PartialEq)]
struct Token(Location, TokenData);

impl Token {
	fn to_string(
		&self,
		file: &str,
		chars: &[char],
		idents: &[String],
		strings: &[String],
	) -> String {
		(match self.1 {
			TokenData::Identifier(i) => idents[i].clone(),
			TokenData::String(i) => strings[i].clone(),
			TokenData::Number(n) => format!("{}", n),
			TokenData::OpenBracket(b) => match b {
				Bracket::Round => "(".to_owned(),
				Bracket::Curly => "{".to_owned(),
				Bracket::Square => "[".to_owned(),
			},
			TokenData::CloseBracket(b) => match b {
				Bracket::Round => ")".to_owned(),
				Bracket::Curly => "}".to_owned(),
				Bracket::Square => "]".to_owned(),
			},
		}) + " at " + &self.0.to_string(file, chars)
	}
}

#[derive(Debug, PartialEq)]
struct TokenizerOutput {
	tokens: Vec<Token>,
	idents: Vec<String>,
	strings: Vec<String>,
}

impl TokenizerOutput {
	fn get_ident(&mut self, ident: &[char]) -> usize {
		let s = ident.iter().collect();
		match self.idents.iter().position(|i| *i == s) {
			Some(i) => i,
			None => {
				self.idents.push(s);
				self.idents.len() - 1
			}
		}
	}
}

fn tokenize(chars: &[char]) -> Result<TokenizerOutput, Error> {
	use TokenData::{CloseBracket, OpenBracket};
	fn search<T, F: Fn(&T) -> bool>(slice: &[T], start: usize, f: F) -> usize {
		slice[start..].iter().position(f).unwrap_or(slice.len() - start) + start
	}
	let mut o = TokenizerOutput { tokens: vec![], idents: vec![], strings: vec![] };
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'"' => {
				i += 1;
				let idx = i;
				let mut s = "".to_owned();
				while !matches!(chars.get(i), Some('"')) {
					match chars.get(i) {
						Some('\\') => {
							i += 1;
							match chars.get(i) {
								Some('\\') => s.push('\\'),
								Some('"') => s.push('\"'),
								Some('n') => s.push('\n'),
								Some('t') => s.push('\t'),
								Some('u') => todo!(),
								_ => Err(Error::InvalidEscapeCharacter(Location::new(i, 1)))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(Error::MissingEndQuote(Location::new(i, 0)))?,
					}
					i += 1;
				}
				o.strings.push(s);
				o.tokens.push(Token(
					Location::new(idx, i - idx),
					TokenData::String(o.strings.len() - 1),
				));
			}
			'#' => i = search(chars, i, |c| *c == '\n'),
			'(' => o.tokens.push(Token(Location::new(i, 1), OpenBracket(Bracket::Round))),
			'{' => o.tokens.push(Token(Location::new(i, 1), OpenBracket(Bracket::Curly))),
			'[' => o.tokens.push(Token(Location::new(i, 1), OpenBracket(Bracket::Square))),
			')' => o.tokens.push(Token(Location::new(i, 1), CloseBracket(Bracket::Round))),
			'}' => o.tokens.push(Token(Location::new(i, 1), CloseBracket(Bracket::Curly))),
			']' => o.tokens.push(Token(Location::new(i, 1), CloseBracket(Bracket::Square))),
			' ' | '\t' | '\n' => {}
			_ => {
				let end = search(chars, i, |c| {
					matches!(
						c,
						'"' | '#' | '(' | '{' | '[' | ')' | '}' | ']' | ' ' | '\t' | '\n'
					)
				});
				let data =
					match chars[i..end].iter().enumerate().rev().try_fold((0, 1), |(a, b), t| {
						match t {
							(0, '-') => Some((-a, b)),
							(_, '0') => Some((a, b * 10)),
							(_, '1') => Some((a + b, b * 10)),
							(_, '2') => Some((a + b * 2, b * 10)),
							(_, '3') => Some((a + b * 3, b * 10)),
							(_, '4') => Some((a + b * 4, b * 10)),
							(_, '5') => Some((a + b * 5, b * 10)),
							(_, '6') => Some((a + b * 6, b * 10)),
							(_, '7') => Some((a + b * 7, b * 10)),
							(_, '8') => Some((a + b * 8, b * 10)),
							(_, '9') => Some((a + b * 9, b * 10)),
							(_, '_') => Some((a, b)),
							_ => None,
						}
					}) {
						Some((n, _)) => TokenData::Number(n),
						None => TokenData::Identifier(o.get_ident(&chars[i..end])),
					};
				o.tokens.push(Token(Location::new(i, end - i), data));
				i = end - 1;
			}
		}
		i += 1;
	}
	Ok(o)
}

#[test]
fn tokenizer_test() {
	use TokenData::*;
	let test_err = |a: &str, b| assert_eq!(tokenize(&a.chars().collect::<Vec<char>>()), Err(b));
	let test_ok = |a: &str, tokens, idents, strings| {
		assert_eq!(
			tokenize(&a.chars().collect::<Vec<char>>()),
			Ok(TokenizerOutput { tokens, idents, strings })
		)
	};
	test_err("\"\"\"", Error::MissingEndQuote(Location::new(3, 0)));
	test_err("\"\\", Error::InvalidEscapeCharacter(Location::new(2, 1)));
	test_err("\"\\a", Error::InvalidEscapeCharacter(Location::new(2, 1)));
	test_ok(
		"214s2135**adfe2",
		vec![Token(Location::new(0, 15), Identifier(0))],
		vec!["214s2135**adfe2".to_owned()],
		vec![],
	);
	test_ok("\"\"", vec![Token(Location::new(1, 0), String(0))], vec![], vec!["".to_owned()]);
	test_ok("-5_84_39", vec![Token(Location::new(0, 8), Number(-58439))], vec![], vec![]);
	test_ok(")", vec![Token(Location::new(0, 1), CloseBracket(Bracket::Round))], vec![], vec![]);
	test_ok(
		"a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})",
		vec![
			Token(Location::new(0, 1), Identifier(0)),
			Token(Location::new(2, 1), Identifier(1)),
			Token(Location::new(5, 35), String(0)),
			Token(Location::new(42, 1), Identifier(2)),
			Token(Location::new(44, 1), Identifier(1)),
			Token(Location::new(46, 10), Number(-1000000)),
			Token(Location::new(57, 1), OpenBracket(Bracket::Round)),
			Token(Location::new(58, 1), Identifier(3)),
			Token(Location::new(60, 1), Identifier(1)),
			Token(Location::new(62, 1), OpenBracket(Bracket::Curly)),
			Token(Location::new(63, 1), Identifier(0)),
			Token(Location::new(64, 1), CloseBracket(Bracket::Curly)),
			Token(Location::new(65, 1), CloseBracket(Bracket::Round)),
		],
		vec!["a".to_owned(), "=".to_owned(), "b".to_owned(), "c".to_owned()],
		vec!["text with  s, \ts, \ns, \"s, and \\s".to_owned()],
	);
}
