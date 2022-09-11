#[derive(Debug, PartialEq)]
enum Error<'a> {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
	MissingEndQuote(Location<'a>),
	InvalidEscapeCharacter(Location<'a>),
	MissingCloseBracket(Location<'a>),
	ExtraCloseBracket(Location<'a>),
	NotEnoughArgs(Location<'a>),
}

impl std::fmt::Display for Error<'_> {
	fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
		todo!("error display")
	}
}

fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = match args.get(1) {
		None => {
			println!("\n{}", Error::MissingCommandLineArgument);
			return;
		}
		Some(file) => file,
	};
	let chars: Vec<char> = match std::fs::read_to_string(file) {
		Err(_) => {
			println!("\n{}", Error::CouldNotReadFile(file.to_owned()));
			return;
		}
		Ok(chars) => chars.chars().collect(),
	};
	let t = match tokenize(&chars) {
		Err(e) => {
			println!("\n{}", e);
			return;
		}
		Ok(t) => t,
	};
	let p = match parse(&t) {
		Err(e) => {
			println!("\n{}", e);
			return;
		}
		Ok(p) => p,
	};
	println!("\n{}", p);
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Location<'a> {
	chars: &'a [char],
	idx: usize,
	len: usize,
}

fn loc(idx: usize, len: usize, chars: &[char]) -> Location {
	Location { idx, len, chars }
}

impl std::fmt::Display for Location<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let fold = |(row, col), &c| match c {
			'\n' => (row + 1, 1),
			_ => (row, col + 1),
		};
		let (row0, col0) = self.chars[..self.idx].iter().fold((1, 1), fold);
		let (row1, col1) = self.to_chars().iter().fold((row0, col0), fold);
		write!(f, "{}:{}-{}:{}", row0, col0, row1, col1)
	}
}

impl<'a> Location<'a> {
	fn to_chars(self) -> &'a [char] {
		&self.chars[self.idx..self.idx + self.len]
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token<'a> {
	Ident(Location<'a>),
	String(usize, Location<'a>),
	Number(i64, Location<'a>),
	Opener(Bracket, Location<'a>),
	Closer(Bracket, Location<'a>),
}

#[derive(Debug, PartialEq)]
struct Tokenizer<'a> {
	chars: &'a [char],
	tokens: Vec<Token<'a>>,
	strings: Vec<String>,
}

impl std::fmt::Display for Tokenizer<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for token in &self.tokens {
			let (s, l) = match token {
				Token::Ident(l) => (l.to_chars().iter().collect::<String>(), l),
				Token::String(i, l) => (self.strings[*i].to_owned(), l),
				Token::Number(n, l) => (format!("{}", n), l),
				Token::Opener(Bracket::Round, l) => ("(".to_owned(), l),
				Token::Opener(Bracket::Curly, l) => ("{".to_owned(), l),
				Token::Opener(Bracket::Square, l) => ("[".to_owned(), l),
				Token::Closer(Bracket::Round, l) => (")".to_owned(), l),
				Token::Closer(Bracket::Curly, l) => ("}".to_owned(), l),
				Token::Closer(Bracket::Square, l) => ("]".to_owned(), l),
			};
			writeln!(f, "{} at {}", s, l)?
		}
		Ok(())
	}
}

fn tokenize(chars: &[char]) -> Result<Tokenizer, Error> {
	fn search<F: Fn(&char) -> bool>(chars: &[char], start: usize, f: F) -> usize {
		chars[start..].iter().position(f).unwrap_or(chars.len() - start) + start
	}
	let mut t = Tokenizer { chars, tokens: vec![], strings: vec![] };
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
								_ => Err(Error::InvalidEscapeCharacter(loc(i, 0, chars)))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(Error::MissingEndQuote(loc(i, 0, chars)))?,
					}
					i += 1;
				}
				t.strings.push(s);
				t.tokens.push(Token::String(t.strings.len() - 1, loc(idx, i - idx, chars)));
			}
			'#' => i = search(chars, i, |c| *c == '\n'),
			'(' => t.tokens.push(Token::Opener(Bracket::Round, loc(i, 1, chars))),
			')' => t.tokens.push(Token::Closer(Bracket::Round, loc(i, 1, chars))),
			'{' => t.tokens.push(Token::Opener(Bracket::Curly, loc(i, 1, chars))),
			'}' => t.tokens.push(Token::Closer(Bracket::Curly, loc(i, 1, chars))),
			'[' => t.tokens.push(Token::Opener(Bracket::Square, loc(i, 1, chars))),
			']' => t.tokens.push(Token::Closer(Bracket::Square, loc(i, 1, chars))),
			' ' | '\t' | '\n' => {}
			_ => {
				let l = loc(
					i,
					search(chars, i, |a| "\"#(){}{} \t\n".chars().any(|b| *a == b)) - i,
					chars,
				);
				t.tokens.push(
					match l.to_chars().iter().enumerate().rev().try_fold((0, 1), |(a, b), t| {
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
						_ if l.to_chars().iter().all(|c| !c.is_ascii_digit()) => Token::Ident(l),
						Some((n, _)) => Token::Number(n, l),
						None => Token::Ident(l),
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
fn tokenize_test() {
	let chars = "\"\"\"".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars), Err(Error::MissingEndQuote(loc(3, 0, &chars))));
	let chars = "\"\\".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars), Err(Error::InvalidEscapeCharacter(loc(2, 0, &chars))));
	let chars = "\"\\a".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars), Err(Error::InvalidEscapeCharacter(loc(2, 0, &chars))));
	let chars = "214s2135**adfe2".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars),
		Ok(Tokenizer {
			tokens: vec![Token::Ident(loc(0, 15, &chars))],
			strings: vec![],
			chars: &chars
		})
	);
	let chars = "\"\"".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars),
		Ok(Tokenizer {
			tokens: vec![Token::String(0, loc(1, 0, &chars))],
			strings: vec!["".to_owned()],
			chars: &chars
		})
	);
	let chars = "-5_84_39".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars),
		Ok(Tokenizer {
			tokens: vec![Token::Number(-58439, loc(0, 8, &chars))],
			strings: vec![],
			chars: &chars
		})
	);
	let chars = ")".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars),
		Ok(Tokenizer {
			tokens: vec![Token::Closer(Bracket::Round, loc(0, 1, &chars))],
			strings: vec![],
			chars: &chars
		})
	);
	let chars = "a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})"
		.chars()
		.collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars),
		Ok(Tokenizer {
			tokens: vec![
				Token::Ident(loc(0, 1, &chars)),
				Token::Ident(loc(2, 1, &chars)),
				Token::String(0, loc(5, 35, &chars)),
				Token::Ident(loc(42, 1, &chars)),
				Token::Ident(loc(44, 1, &chars)),
				Token::Number(-1000000, loc(46, 10, &chars)),
				Token::Opener(Bracket::Round, loc(57, 1, &chars)),
				Token::Ident(loc(58, 1, &chars)),
				Token::Ident(loc(60, 1, &chars)),
				Token::Opener(Bracket::Curly, loc(62, 1, &chars)),
				Token::Ident(loc(63, 1, &chars)),
				Token::Closer(Bracket::Curly, loc(64, 1, &chars)),
				Token::Closer(Bracket::Round, loc(65, 1, &chars)),
			],
			strings: vec!["text with  s, \ts, \ns, \"s, and \\s".to_owned()],
			chars: &chars
		})
	);
}

#[derive(PartialEq)]
struct Operator {
	name: &'static str,
	func: &'static str,
	left: usize,
	right: usize,
}

const fn op(name: &'static str, func: &'static str, left: usize, right: usize) -> Operator {
	Operator { name, func, left, right }
}

// Grouped by precedence; highest first
// bool is right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
	(&[op("*", "mul", 1, 1)], false),
	(&[op("+", "add", 1, 1), op("-", "sub", 1, 1)], false),
	(&[op("->", "arrow", 1, 1)], true),
];

#[derive(Clone, Debug, PartialEq)]
enum AST<'a> {
	Ident(Location<'a>),
	String(usize, Location<'a>),
	Number(i64, Location<'a>),
	Brackets(Bracket, Location<'a>, Location<'a>, Vec<AST<'a>>),
	Operator((usize, usize), Location<'a>, Vec<AST<'a>>),
}

#[derive(Debug, PartialEq)]
struct Parser<'a> {
	tokenizer: &'a Tokenizer<'a>,
	asts: Vec<AST<'a>>,
}

impl std::fmt::Display for Parser<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		fn write_ast(
			ast: &AST,
			f: &mut std::fmt::Formatter,
			depth: usize,
			t: &Tokenizer,
		) -> std::fmt::Result {
			let (s, l) = match &ast {
				AST::Ident(l) => (l.to_chars().iter().collect::<String>(), l),
				AST::String(i, l) => (t.strings[*i].to_owned(), l),
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
						l,
						k
					)?;
					return asts.iter().try_fold((), |_, ast| write_ast(ast, f, depth + 1, t));
				}
				AST::Operator((i, j), l, asts) => {
					writeln!(f, "{}{} at {}", "\t".repeat(depth), OPERATORS[*i].0[*j].func, l,)?;
					return asts.iter().try_fold((), |_, ast| write_ast(ast, f, depth + 1, t));
				}
			};
			writeln!(f, "{}{} at {}", "\t".repeat(depth), s, l)
		}
		self.asts.iter().try_fold((), |_, ast| write_ast(ast, f, 0, self.tokenizer))
	}
}

fn parse<'a>(tokenizer: &'a Tokenizer<'_>) -> Result<Parser<'a>, Error<'a>> {
	fn consume_up_to<'a>(
		tokens: &[Token<'a>],
		i: &mut usize,
		end: Option<Bracket>,
		t: &Tokenizer<'a>,
	) -> Result<Vec<AST<'a>>, Error<'a>> {
		let mut asts = vec![];
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => {
					Err(Error::MissingCloseBracket(loc(t.chars.len(), 0, t.chars)))?
				}
				(Some(end), Some(Token::Closer(b, l))) if end != *b => {
					Err(Error::ExtraCloseBracket(*l))?
				}
				(None, Some(Token::Closer(_, l))) => Err(Error::ExtraCloseBracket(*l))?,
				(Some(_), Some(Token::Closer(_, _))) => break,
				(None, None) => break,
				(_, Some(Token::Ident(l))) => AST::Ident(*l),
				(_, Some(Token::String(s, l))) => AST::String(*s, *l),
				(_, Some(Token::Number(n, l))) => AST::Number(*n, *l),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let asts = consume_up_to(tokens, i, Some(*b), t)?;
					let k = match tokens[*i] {
						Token::Ident(l) => l,
						Token::String(_, l) => l,
						Token::Number(_, l) => l,
						Token::Opener(_, l) => l,
						Token::Closer(_, l) => l,
					};
					AST::Brackets(*b, *l, k, asts)
				}
			});
			*i += 1;
		}
		for (a, (ops, right)) in OPERATORS.iter().enumerate() {
			let mut j = if *right { asts.len().wrapping_sub(1) } else { 0 };
			while let Some(ast) = asts.get(j) {
				if let AST::Ident(l) = ast.clone() {
					if let Some((b, op)) = ops
						.iter()
						.enumerate()
						.find(|(_, op)| op.name == l.to_chars().iter().collect::<String>())
					{
						if j < op.left || j + op.right >= asts.len() {
							Err(Error::NotEnoughArgs(l))?
						}
						asts.remove(j);
						let c: Vec<AST> = asts.drain(j - op.left..j + op.right).collect();
						j -= op.left;
						asts.insert(j, AST::Operator((a, b), l, c));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		Ok(asts)
	}
	Ok(Parser { tokenizer, asts: consume_up_to(&tokenizer.tokens, &mut 0, None, tokenizer)? })
}

#[test]
fn parse_test() {
	use AST::*; // todo: keep?

	let chars: Vec<char> = "{".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()),
		Err(Error::MissingCloseBracket(loc(1, 0, &chars)))
	);
	let chars: Vec<char> = ")".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()),
		Err(Error::ExtraCloseBracket(loc(0, 1, &chars)))
	);

	let chars: Vec<char> = "({)}".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()),
		Err(Error::ExtraCloseBracket(loc(2, 1, &chars)))
	);

	let chars: Vec<char> = "{}".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Brackets(Bracket::Curly, loc(0, 1, &chars), loc(1, 1, &chars), vec![])]
		})
	);

	let chars: Vec<char> = "[{   }]".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Brackets(
				Bracket::Square,
				loc(0, 1, &chars),
				loc(6, 1, &chars),
				vec![Brackets(Bracket::Curly, loc(1, 1, &chars), loc(5, 1, &chars), vec![])],
			)]
		})
	);

	let chars: Vec<char> = "+ 1".chars().collect();
	assert_eq!(parse(&tokenize(&chars).unwrap()), Err(Error::NotEnoughArgs(loc(0, 1, &chars))));

	let chars: Vec<char> = "1 ->".chars().collect();
	assert_eq!(parse(&tokenize(&chars).unwrap()), Err(Error::NotEnoughArgs(loc(2, 2, &chars))));

	let chars: Vec<char> = "a + b".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Operator(
				(1, 0),
				loc(2, 1, &chars),
				vec![Ident(loc(0, 1, &chars)), Ident(loc(4, 1, &chars))]
			)]
		})
	);

	let chars: Vec<char> = "a - b * c".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Operator(
				(1, 1),
				loc(2, 1, &chars),
				vec![
					Ident(loc(0, 1, &chars)),
					Operator(
						(0, 0),
						loc(6, 1, &chars),
						vec![Ident(loc(4, 1, &chars)), Ident(loc(8, 1, &chars))]
					),
				],
			)]
		})
	);

	let chars: Vec<char> = "a *  b *  c".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Operator(
				(0, 0),
				loc(7, 1, &chars),
				vec![
					Operator(
						(0, 0),
						loc(2, 1, &chars),
						vec![Ident(loc(0, 1, &chars)), Ident(loc(5, 1, &chars))],
					),
					Ident(loc(10, 1, &chars)),
				],
			)]
		})
	);

	let chars: Vec<char> = "a -> b -> c".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Operator(
				(2, 0),
				loc(2, 2, &chars),
				vec![
					Ident(loc(0, 1, &chars)),
					Operator(
						(2, 0),
						loc(7, 2, &chars),
						vec![Ident(loc(5, 1, &chars)), Ident(loc(10, 1, &chars))],
					),
				],
			)]
		})
	);

	let chars: Vec<char> = "[(a) + ({3 * 97})]".chars().collect();
	let t = tokenize(&chars).unwrap();
	assert_eq!(
		parse(&t),
		Ok(Parser {
			tokenizer: &t,
			asts: vec![Brackets(
				Bracket::Square,
				loc(0, 1, &chars),
				loc(17, 1, &chars),
				vec![Operator(
					(1, 0),
					loc(5, 1, &chars),
					vec![
						Brackets(
							Bracket::Round,
							loc(1, 1, &chars),
							loc(3, 1, &chars),
							vec![Ident(loc(2, 1, &chars))],
						),
						Brackets(
							Bracket::Round,
							loc(7, 1, &chars),
							loc(16, 1, &chars),
							vec![Brackets(
								Bracket::Curly,
								loc(8, 1, &chars),
								loc(15, 1, &chars),
								vec![Operator(
									(0, 0),
									loc(11, 1, &chars),
									vec![
										Number(3, loc(9, 1, &chars)),
										Number(97, loc(13, 2, &chars)),
									],
								)],
							)],
						),
					],
				)],
			)],
		})
	);
}
