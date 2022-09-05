#[derive(PartialEq, Debug)]
enum Error {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
	MissingEndQuote(Location),
	MissingEscapeCharacter(Location),
	InvalidEscapeCharacter(Location),
}

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
	let text =
		std::fs::read_to_string(file).map_err(|_| Error::CouldNotReadFile(file.to_owned()))?;
	let chars: Vec<char> = text.chars().collect();

	let tokens = tokenize(&chars)?;
	for token in tokens {
		println!("{}", token.to_string(file, &chars));
	}

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
		let mut row = 1;
		let mut col = 1;
		for c in chars {
			if *c == '\n' {
				row += 1;
				col = 1;
			} else {
				col += 1;
			}
		}
		format!(" @ {}:{} in {}", row, col, file)
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
	Identifier,
	String,
	Number(i64),
	OpenBracket(Bracket),
	CloseBracket(Bracket),
}

#[derive(Debug, PartialEq)]
struct Token(Location, TokenData);

impl Token {
	fn to_string(&self, file: &str, chars: &[char]) -> String {
		chars[self.0.idx..self.0.idx + self.0.len].iter().collect::<String>()
			+ &self.0.to_string(file, chars)
	}
}

fn tokenize(chars: &[char]) -> Result<Vec<Token>, Error> {
	let mut out = vec![];
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'"' => {
				i += 1;
				let mut j = 0;
				loop {
					match chars.get(i + j) {
						Some('"') => break,
						Some('\\') => {
							j += 1;
							match chars.get(i + j) {
								Some('\\') | Some('"') | Some('n') | Some('t') => {}
								Some(_) => {
									Err(Error::InvalidEscapeCharacter(Location::new(i + j, 1)))?
								}
								None => {
									Err(Error::MissingEscapeCharacter(Location::new(i + j, 0)))?
								}
							}
						}
						Some(_) => {}
						None => Err(Error::MissingEndQuote(Location::new(i + j, 0)))?,
					}
					j += 1;
				}
				out.push(Token(Location::new(i, j), TokenData::String));
				i += j;
			}
			'#' => {
				i += 1;
				while i < chars.len() && chars[i] != '\n' {
					i += 1;
				}
			}
			'(' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Round))),
			'{' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Curly))),
			'[' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Square))),
			')' => out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Round))),
			'}' => out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Curly))),
			']' => {
				out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Square)))
			}
			' ' | '\t' | '\n' => {}
			_ => {
				let idx = i;
				while i < chars.len()
					&& !matches!(
						chars[i],
						'"' | '#' | '(' | '{' | '[' | ')' | '}' | ']' | ' ' | '\t' | '\n'
					) {
					i += 1;
				}

				let mut a = 0i64;
				let mut b = 1;
				let mut fail = false;
				for (i, c) in chars[idx..i].iter().enumerate().rev() {
					if i == 0 && *c == '-' {
						a *= -1;
					} else if let Some(c) = "0123456789".chars().position(|b| *c == b) {
						a += b * c as i64;
						b *= 10;
					} else if *c != '_' {
						fail = true;
						break;
					}
				}
				let data = if fail { TokenData::Identifier } else { TokenData::Number(a) };
				out.push(Token(Location::new(idx, i - idx), data));

				i -= 1;
			}
		}
		i += 1;
	}
	Ok(out)
}

#[test]
fn tokenizer_test() {
	let test = |a: &str, b| assert_eq!(tokenize(&a.chars().collect::<Vec<char>>()), b);
	test("\"\"\"", Err(Error::MissingEndQuote(Location::new(3, 0))));
	test("\"\\", Err(Error::MissingEscapeCharacter(Location::new(2, 0))));
	test("\"\\a", Err(Error::InvalidEscapeCharacter(Location::new(2, 1))));
	test("214s2135**adfe442341", Ok(vec![Token(Location::new(0, 20), TokenData::Identifier)]));
	test("\"\"", Ok(vec![Token(Location::new(1, 0), TokenData::String)]));
	test("-5_84_39", Ok(vec![Token(Location::new(0, 8), TokenData::Number(-58439))]));
	test(")", Ok(vec![Token(Location::new(0, 1), TokenData::CloseBracket(Bracket::Round))]));
	test(
		"a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})",
		Ok(vec![
			Token(Location::new(0, 1), TokenData::Identifier),
			Token(Location::new(2, 1), TokenData::Identifier),
			Token(Location::new(5, 35), TokenData::String),
			Token(Location::new(42, 1), TokenData::Identifier),
			Token(Location::new(44, 1), TokenData::Identifier),
			Token(Location::new(46, 10), TokenData::Number(-1000000)),
			Token(Location::new(57, 1), TokenData::OpenBracket(Bracket::Round)),
			Token(Location::new(58, 1), TokenData::Identifier),
			Token(Location::new(60, 1), TokenData::Identifier),
			Token(Location::new(62, 1), TokenData::OpenBracket(Bracket::Curly)),
			Token(Location::new(63, 1), TokenData::Identifier),
			Token(Location::new(64, 1), TokenData::CloseBracket(Bracket::Curly)),
			Token(Location::new(65, 1), TokenData::CloseBracket(Bracket::Round)),
		]),
	);
}
