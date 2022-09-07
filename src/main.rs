#[derive(PartialEq, Debug)]
enum Error {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
	MissingEndQuote(String),
	InvalidEscapeCharacter(String),
}

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
	let text =
		std::fs::read_to_string(file).map_err(|_| Error::CouldNotReadFile(file.to_owned()))?;
	let chars: Vec<char> = text.chars().collect();

	let t = tokenize(&chars, Some(file))?;

	println!();
	for i in 0..t.tokens.len() {
		println!("{}", t.token_to_string(i));
	}
	println!();
	Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Location {
	idx: usize,
	len: usize,
}

fn loc(idx: usize, len: usize) -> Location {
	Location { idx, len }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token {
	Ident(usize),
	String(usize),
	Number(i64),
	Opener(Bracket),
	Closer(Bracket),
}

#[derive(Debug, PartialEq)]
struct Tokenizer<'a> {
	tokens: Vec<(Token, Location)>,
	idents: Vec<Location>,
	strings: Vec<String>,
	chars: &'a [char],
	file: Option<&'a str>,
}

impl<'a> std::ops::Index<Location> for Tokenizer<'a> {
	type Output = [char];
	fn index(&self, l: Location) -> &[char] {
		&self.chars[l.idx..l.idx + l.len]
	}
}

impl<'a> Tokenizer<'a> {
	fn search<F: Fn(&char) -> bool>(&self, start: usize, f: F) -> usize {
		self.chars[start..].iter().position(f).unwrap_or(self.chars.len() - start) + start
	}

	fn index_to_string(&self, index: usize) -> String {
		let (row, col) = self.chars[..index].iter().fold((1, 1), |(row, col), c| match c {
			'\n' => (row + 1, 1),
			_ => (row, col + 1),
		});
		match self.file {
			Some(file) => format!("{}:{}:{}", file, row, col),
			None => format!("{}:{}", row, col),
		}
	}

	fn token_to_string(&self, token: usize) -> String {
		(match self.tokens[token].0 {
			Token::Ident(i) => self[self.idents[i]].iter().collect(),
			Token::String(i) => self.strings[i].clone(),
			Token::Number(n) => format!("{}", n),
			Token::Opener(Bracket::Round) => "(".to_owned(),
			Token::Opener(Bracket::Curly) => "{".to_owned(),
			Token::Opener(Bracket::Square) => "[".to_owned(),
			Token::Closer(Bracket::Round) => ")".to_owned(),
			Token::Closer(Bracket::Curly) => "}".to_owned(),
			Token::Closer(Bracket::Square) => "]".to_owned(),
		}) + " at " + &self.index_to_string(self.tokens[token].1.idx)
	}
}

fn tokenize<'a>(chars: &'a [char], file: Option<&'a str>) -> Result<Tokenizer<'a>, Error> {
	let mut t = Tokenizer { tokens: vec![], idents: vec![], strings: vec![], chars, file };
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
								_ => Err(Error::InvalidEscapeCharacter(t.index_to_string(i)))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(Error::MissingEndQuote(t.index_to_string(i)))?,
					}
					i += 1;
				}
				t.strings.push(s);
				t.tokens.push((Token::String(t.strings.len() - 1), loc(idx, i - idx)));
			}
			'#' => i = t.search(i, |c| *c == '\n'),
			'(' => t.tokens.push((Token::Opener(Bracket::Round), loc(i, 1))),
			')' => t.tokens.push((Token::Closer(Bracket::Round), loc(i, 1))),
			'{' => t.tokens.push((Token::Opener(Bracket::Curly), loc(i, 1))),
			'}' => t.tokens.push((Token::Closer(Bracket::Curly), loc(i, 1))),
			'[' => t.tokens.push((Token::Opener(Bracket::Square), loc(i, 1))),
			']' => t.tokens.push((Token::Closer(Bracket::Square), loc(i, 1))),
			' ' | '\t' | '\n' => {}
			_ => {
				let l = loc(i, t.search(i, |a| "\"#(){}{} \t\n".chars().any(|b| *a == b)) - i);
				t.tokens.push((
					match t[l].iter().enumerate().rev().try_fold((0, 1), |(a, b), t| match t {
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
					}) {
						Some((n, _)) => Token::Number(n),
						None => Token::Ident(
							t.idents.iter().position(|i| t[*i] == t[l]).unwrap_or_else(|| {
								t.idents.push(l);
								t.idents.len() - 1
							}),
						),
					},
					l,
				));
				i += l.len - 1;
			}
		}
		i += 1;
	}
	Ok(t)
}

#[test]
fn tokenizer_test() {
	let test_err =
		|a: &str, b| assert_eq!(tokenize(&a.chars().collect::<Vec<char>>(), None), Err(b));
	let test_ok = |s: &str, tokens, idents, strings| {
		let chars = &s.chars().collect::<Vec<char>>();
		assert_eq!(
			tokenize(chars, None),
			Ok(Tokenizer { tokens, idents, strings, file: None, chars })
		)
	};
	test_err("\"\"\"", Error::MissingEndQuote("1:4".to_owned()));
	test_err("\"\\", Error::InvalidEscapeCharacter("1:3".to_owned()));
	test_err("\"\\a", Error::InvalidEscapeCharacter("1:3".to_owned()));
	test_ok("214s2135**adfe2", vec![(Token::Ident(0), loc(0, 15))], vec![loc(0, 15)], vec![]);
	test_ok("\"\"", vec![(Token::String(0), loc(1, 0))], vec![], vec!["".to_owned()]);
	test_ok("-5_84_39", vec![(Token::Number(-58439), loc(0, 8))], vec![], vec![]);
	test_ok(")", vec![(Token::Closer(Bracket::Round), loc(0, 1))], vec![], vec![]);
	test_ok(
		"a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})",
		vec![
			(Token::Ident(0), loc(0, 1)),
			(Token::Ident(1), loc(2, 1)),
			(Token::String(0), loc(5, 35)),
			(Token::Ident(2), loc(42, 1)),
			(Token::Ident(1), loc(44, 1)),
			(Token::Number(-1000000), loc(46, 10)),
			(Token::Opener(Bracket::Round), loc(57, 1)),
			(Token::Ident(3), loc(58, 1)),
			(Token::Ident(1), loc(60, 1)),
			(Token::Opener(Bracket::Curly), loc(62, 1)),
			(Token::Ident(0), loc(63, 1)),
			(Token::Closer(Bracket::Curly), loc(64, 1)),
			(Token::Closer(Bracket::Round), loc(65, 1)),
		],
		vec![loc(0, 1), loc(2, 1), loc(42, 1), loc(58, 1)],
		vec!["text with  s, \ts, \ns, \"s, and \\s".to_owned()],
	);
}
