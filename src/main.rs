#[derive(PartialEq, Debug)]
enum Error {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
	MissingEndQuote(String),
	InvalidEscapeCharacter(String),
	MissingCloseBracket(String),
	ExtraCloseBracket(String),
}

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
	let chars: Vec<char> = std::fs::read_to_string(file)
		.map_err(|_| Error::CouldNotReadFile(file.to_owned()))?
		.chars()
		.collect();
	let input = Input { chars: &chars, file: Some(file) };

	let t = tokenize(&input)?;
	let p = parse(&t)?;
	println!("\n{:?}\n", p);
	Ok(())
}

#[derive(Debug, PartialEq)]
struct Input<'a> {
	chars: &'a [char],
	file: Option<&'a str>,
}

impl<'a> Input<'a> {
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
	Ident(usize, Location),
	String(usize, Location),
	Number(i64, Location),
	Opener(Bracket, Location),
	Closer(Bracket, Location),
}

#[derive(PartialEq)]
struct Tokenizer<'a> {
	input: &'a Input<'a>,
	tokens: Vec<Token>,
	idents: Vec<Location>,
	strings: Vec<String>,
}

impl<'a> std::fmt::Debug for Tokenizer<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for token in &self.tokens {
			let (s, l) = match token {
				Token::Ident(i, l) => (self[self.idents[*i]].iter().collect::<String>(), l),
				Token::String(i, l) => (self.strings[*i].to_owned(), l),
				Token::Number(n, l) => (format!("{}", n), l),
				Token::Opener(Bracket::Round, l) => ("(".to_owned(), l),
				Token::Opener(Bracket::Curly, l) => ("{".to_owned(), l),
				Token::Opener(Bracket::Square, l) => ("[".to_owned(), l),
				Token::Closer(Bracket::Round, l) => (")".to_owned(), l),
				Token::Closer(Bracket::Curly, l) => ("}".to_owned(), l),
				Token::Closer(Bracket::Square, l) => ("]".to_owned(), l),
			};
			writeln!(f, "{} at {}", s, self.input.index_to_string(l.idx))?
		}
		Ok(())
	}
}

impl<'a> std::ops::Index<Location> for Tokenizer<'a> {
	type Output = [char];
	fn index(&self, l: Location) -> &[char] {
		&self.input.chars[l.idx..l.idx + l.len]
	}
}

fn tokenize<'a>(input: &'a Input<'a>) -> Result<Tokenizer<'a>, Error> {
	let mut t = Tokenizer { tokens: vec![], idents: vec![], strings: vec![], input };
	let mut i = 0;
	while let Some(c) = input.chars.get(i) {
		match c {
			'"' => {
				i += 1;
				let idx = i;
				let mut s = "".to_owned();
				loop {
					match input.chars.get(i) {
						Some('"') => break,
						Some('\\') => {
							i += 1;
							match input.chars.get(i) {
								Some('\\') => s.push('\\'),
								Some('"') => s.push('\"'),
								Some('n') => s.push('\n'),
								Some('t') => s.push('\t'),
								_ => {
									Err(Error::InvalidEscapeCharacter(input.index_to_string(i)))?
								}
							}
						}
						Some(c) => s.push(*c),
						None => Err(Error::MissingEndQuote(input.index_to_string(i)))?,
					}
					i += 1;
				}
				t.strings.push(s);
				t.tokens.push(Token::String(t.strings.len() - 1, loc(idx, i - idx)));
			}
			'#' => i = input.search(i, |c| *c == '\n'),
			'(' => t.tokens.push(Token::Opener(Bracket::Round, loc(i, 1))),
			')' => t.tokens.push(Token::Closer(Bracket::Round, loc(i, 1))),
			'{' => t.tokens.push(Token::Opener(Bracket::Curly, loc(i, 1))),
			'}' => t.tokens.push(Token::Closer(Bracket::Curly, loc(i, 1))),
			'[' => t.tokens.push(Token::Opener(Bracket::Square, loc(i, 1))),
			']' => t.tokens.push(Token::Closer(Bracket::Square, loc(i, 1))),
			' ' | '\t' | '\n' => {}
			_ => {
				let l =
					loc(i, input.search(i, |a| "\"#(){}{} \t\n".chars().any(|b| *a == b)) - i);
				t.tokens.push(
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
						Some((n, _)) => Token::Number(n, l),
						None => Token::Ident(
							t.idents.iter().position(|i| t[*i] == t[l]).unwrap_or_else(|| {
								t.idents.push(l);
								t.idents.len() - 1
							}),
							l,
						),
					},
				);
				i += l.len - 1;
			}
		}
		i += 1;
	}
	Ok(t)
}

#[test]
fn tokenizer_test() {
	let test_err = |a: &str, b| {
		assert_eq!(
			tokenize(&Input { chars: &a.chars().collect::<Vec<char>>(), file: None }),
			Err(b)
		)
	};
	let test_ok = |s: &str, tokens, idents, strings| {
		let chars = &s.chars().collect::<Vec<char>>();
		let input = Input { chars, file: None };
		assert_eq!(tokenize(&input), Ok(Tokenizer { tokens, idents, strings, input: &input }))
	};
	test_err("\"\"\"", Error::MissingEndQuote("1:4".to_owned()));
	test_err("\"\\", Error::InvalidEscapeCharacter("1:3".to_owned()));
	test_err("\"\\a", Error::InvalidEscapeCharacter("1:3".to_owned()));
	test_ok("214s2135**adfe2", vec![(Token::Ident(0, loc(0, 15)))], vec![loc(0, 15)], vec![]);
	test_ok("\"\"", vec![(Token::String(0, loc(1, 0)))], vec![], vec!["".to_owned()]);
	test_ok("-5_84_39", vec![(Token::Number(-58439, loc(0, 8)))], vec![], vec![]);
	test_ok(")", vec![(Token::Closer(Bracket::Round, loc(0, 1)))], vec![], vec![]);
	test_ok(
		"a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})",
		vec![
			(Token::Ident(0, loc(0, 1))),
			(Token::Ident(1, loc(2, 1))),
			(Token::String(0, loc(5, 35))),
			(Token::Ident(2, loc(42, 1))),
			(Token::Ident(1, loc(44, 1))),
			(Token::Number(-1000000, loc(46, 10))),
			(Token::Opener(Bracket::Round, loc(57, 1))),
			(Token::Ident(3, loc(58, 1))),
			(Token::Ident(1, loc(60, 1))),
			(Token::Opener(Bracket::Curly, loc(62, 1))),
			(Token::Ident(0, loc(63, 1))),
			(Token::Closer(Bracket::Curly, loc(64, 1))),
			(Token::Closer(Bracket::Round, loc(65, 1))),
		],
		vec![loc(0, 1), loc(2, 1), loc(42, 1), loc(58, 1)],
		vec!["text with  s, \ts, \ns, \"s, and \\s".to_owned()],
	);
}

#[derive(Debug, PartialEq)]
enum AST {
	Ident(usize, Location),
	String(usize, Location),
	Number(i64, Location),
	Brackets(Bracket, Location, Location, Vec<AST>),
}

#[derive(PartialEq)]
struct Parser<'a> {
	tokenizer: &'a Tokenizer<'a>,
	asts: Vec<AST>,
}

impl<'a> std::fmt::Debug for Parser<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		fn write_ast(
			ast: &AST,
			f: &mut std::fmt::Formatter,
			depth: usize,
			tokenizer: &Tokenizer,
		) -> std::fmt::Result {
			let (s, l) = match &ast {
				AST::Ident(i, l) => {
					(tokenizer[tokenizer.idents[*i]].iter().collect::<String>(), l)
				}
				AST::String(i, l) => (tokenizer.strings[*i].to_owned(), l),
				AST::Number(n, l) => (format!("{}", n), l),
				AST::Brackets(b, l, k, asts) => {
					writeln!(
						f,
						"{}{} from {} to {}",
						"\t".repeat(depth),
						match b {
							Bracket::Round => "()",
							Bracket::Curly => "{}",
							Bracket::Square => "[]",
						},
						tokenizer.input.index_to_string(l.idx),
						tokenizer.input.index_to_string(k.idx)
					)?;
					return asts
						.iter()
						.try_fold((), |_, ast| write_ast(ast, f, depth + 1, tokenizer));
				}
			};
			writeln!(
				f,
				"{}{} at {}",
				"\t".repeat(depth),
				s,
				tokenizer.input.index_to_string(l.idx)
			)
		}
		self.asts.iter().try_fold((), |_, ast| write_ast(ast, f, 0, self.tokenizer))
	}
}

fn parse<'a>(tokenizer: &'a Tokenizer<'a>) -> Result<Parser<'a>, Error> {
	fn consume_up_to(
		tokens: &[Token],
		i: &mut usize,
		end: Option<Bracket>,
		input: &Input,
	) -> Result<(Vec<AST>, Location), Error> {
		let mut asts = vec![];
		let k;
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => {
					Err(Error::MissingCloseBracket(input.index_to_string(input.chars.len())))?
				}
				(Some(end), Some(Token::Closer(b, l))) if end != *b => {
					Err(Error::ExtraCloseBracket(input.index_to_string(l.idx)))?
				}
				(None, Some(Token::Closer(_, l))) => {
					Err(Error::ExtraCloseBracket(input.index_to_string(l.idx)))?
				}
				(Some(_), Some(Token::Closer(_, l))) => {
					k = *l;
					break;
				}
				(None, None) => {
					k = loc(input.chars.len(), 0);
					break;
				}
				(_, Some(Token::Ident(i, l))) => AST::Ident(*i, *l),
				(_, Some(Token::String(s, l))) => AST::String(*s, *l),
				(_, Some(Token::Number(n, l))) => AST::Number(*n, *l),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let (asts, k) = consume_up_to(tokens, i, Some(*b), input)?;
					AST::Brackets(*b, *l, k, asts)
				}
			});
			*i += 1;
		}
		Ok((asts, k))
	}
	Ok(Parser {
		tokenizer,
		asts: consume_up_to(&tokenizer.tokens, &mut 0, None, tokenizer.input)?.0,
	})
}

#[test]
fn parser_test() {
	let test_err = |a: &str, b| {
		assert_eq!(
			parse(
				&tokenize(&Input { chars: &a.chars().collect::<Vec<char>>(), file: None })
					.unwrap()
			),
			Err(b)
		)
	};
	let test_ok = |s: &str, asts| {
		assert_eq!(
			parse(
				&tokenize(&Input { chars: &s.chars().collect::<Vec<char>>(), file: None })
					.unwrap()
			)
			.unwrap()
			.asts,
			asts
		)
	};
	test_err("{", Error::MissingCloseBracket("1:2".to_owned()));
	test_err(")", Error::ExtraCloseBracket("1:1".to_owned()));
	test_err("({)}", Error::ExtraCloseBracket("1:3".to_owned()));
	test_ok(
		"[(a) ({3})]",
		vec![AST::Brackets(
			Bracket::Square,
			Location { idx: 0, len: 1 },
			Location { idx: 10, len: 1 },
			vec![
				AST::Brackets(
					Bracket::Round,
					Location { idx: 1, len: 1 },
					Location { idx: 3, len: 1 },
					vec![AST::Ident(0, Location { idx: 2, len: 1 })],
				),
				AST::Brackets(
					Bracket::Round,
					Location { idx: 5, len: 1 },
					Location { idx: 9, len: 1 },
					vec![AST::Brackets(
						Bracket::Curly,
						Location { idx: 6, len: 1 },
						Location { idx: 8, len: 1 },
						vec![AST::Number(3, Location { idx: 7, len: 1 })],
					)],
				),
			],
		)],
	);
}
