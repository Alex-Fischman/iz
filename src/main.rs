fn main() -> Result<(), String> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or_else(|| "no file passed".to_owned())?;
	let chars: Vec<char> = std::fs::read_to_string(file)
		.map_err(|_| format!("could not read {}", file))?
		.chars()
		.collect();
	let tokens = tokenize(&chars)?;
	let asts = parse(&tokens, &chars)?;
	println!("\n{:#?}", asts);
	Ok(())
}

// is there some way to get rid of this char reference?
// not in Rust, but in a non-borrow-checked language it should be a char pointer and a length
#[derive(Clone, Copy, Debug, PartialEq)]
struct Location<'a>(usize, usize, &'a [char]);

impl std::fmt::Display for Location<'_> {
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
	fn to_chars(self) -> &'a [char] {
		&self.2[self.0..self.0 + self.1]
	}
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Debug, PartialEq)]
enum Token<'a> {
	Ident(Location<'a>),
	String(String, Location<'a>),
	Number(i64, Location<'a>),
	Opener(Bracket, Location<'a>),
	Closer(Bracket, Location<'a>),
}

fn tokenize(chars: &[char]) -> Result<Vec<Token>, String> {
	fn search<F: Fn(&char) -> bool>(chars: &[char], start: usize, f: F) -> usize {
		chars[start..].iter().position(f).unwrap_or(chars.len() - start) + start
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
									"no valid escape character at {}",
									Location(i, 0, chars)
								))?,
							}
						}
						Some(c) => s.push(*c),
						None => Err(format!("no end quote at {}", Location(i, 0, chars)))?,
					}
					i += 1;
				}
				t.push(Token::String(s, Location(idx, i - idx, chars)));
			}
			'#' => i = search(chars, i, |c| *c == '\n'),
			'(' => t.push(Token::Opener(Bracket::Round, Location(i, 1, chars))),
			')' => t.push(Token::Closer(Bracket::Round, Location(i, 1, chars))),
			'{' => t.push(Token::Opener(Bracket::Curly, Location(i, 1, chars))),
			'}' => t.push(Token::Closer(Bracket::Curly, Location(i, 1, chars))),
			'[' => t.push(Token::Opener(Bracket::Square, Location(i, 1, chars))),
			']' => t.push(Token::Closer(Bracket::Square, Location(i, 1, chars))),
			' ' | '\t' | '\n' => {}
			_ => {
				let l = Location(
					i,
					search(chars, i, |a| "\"#(){}{} \t\n".chars().any(|b| *a == b)) - i,
					chars,
				);
				t.push(
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
						Some((n, _)) if l.to_chars().iter().any(char::is_ascii_digit) => {
							Token::Number(n, l)
						}
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
	let chars = "214s2135**adfe2".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars).unwrap(), vec![Token::Ident(Location(0, 15, &chars))]);
	let chars = "\"\"".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![Token::String("".to_owned(), Location(1, 0, &chars))]
	);
	let chars = "-5_84_39".chars().collect::<Vec<char>>();
	assert_eq!(tokenize(&chars).unwrap(), vec![Token::Number(-58439, Location(0, 8, &chars))]);
	let chars = ")".chars().collect::<Vec<char>>();
	assert_eq!(
		tokenize(&chars).unwrap(),
		vec![Token::Closer(Bracket::Round, Location(0, 1, &chars))]
	);
	let chars = "a = \"text with  s, \ts, \\ns, \\\"s, and \\\\s\"\nb = -1_000_000 (c = {a})"
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
			Token::Number(-1000000, Location(46, 10, &chars)),
			Token::Opener(Bracket::Round, Location(57, 1, &chars)),
			Token::Ident(Location(58, 1, &chars)),
			Token::Ident(Location(60, 1, &chars)),
			Token::Opener(Bracket::Curly, Location(62, 1, &chars)),
			Token::Ident(Location(63, 1, &chars)),
			Token::Closer(Bracket::Curly, Location(64, 1, &chars)),
			Token::Closer(Bracket::Round, Location(65, 1, &chars)),
		]
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
enum AST<'a, T> {
	Ident(Location<'a>, T),
	String(String, Location<'a>, T),
	Number(i64, Location<'a>, T),
	Brackets(Bracket, Location<'a>, Location<'a>, Vec<AST<'a, T>>, T),
	Operator((usize, usize), Location<'a>, Vec<AST<'a, T>>, T),
}

fn parse<'a>(tokens: &'a [Token<'a>], chars: &'a [char]) -> Result<Vec<AST<'a, ()>>, String> {
	fn parse<'a>(
		tokens: &[Token<'a>],
		chars: &[char],
		i: &mut usize,
		end: Option<Bracket>,
	) -> Result<Vec<AST<'a, ()>>, String> {
		let mut asts = vec![];
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => {
					Err(format!("no close bracket at {}", Location(chars.len(), 0, chars)))?
				}
				(Some(end), Some(Token::Closer(b, _))) if *b == end => break,
				(None, None) => break,
				(_, Some(Token::Closer(_, l))) => Err(format!("extra close bracket at {}", l))?,
				(_, Some(Token::Ident(l))) => AST::Ident(*l, ()),
				(_, Some(Token::String(s, l))) => AST::String(s.clone(), *l, ()),
				(_, Some(Token::Number(n, l))) => AST::Number(*n, *l, ()),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let asts = parse(tokens, chars, i, Some(*b))?;
					let k = match tokens[*i] {
						Token::Ident(l) => l,
						Token::String(_, l) => l,
						Token::Number(_, l) => l,
						Token::Opener(_, l) => l,
						Token::Closer(_, l) => l,
					};
					AST::Brackets(*b, *l, k, asts, ())
				}
			});
			*i += 1;
		}
		for (a, (ops, right)) in OPERATORS.iter().enumerate() {
			let mut j = if *right { asts.len().wrapping_sub(1) } else { 0 };
			while let Some(ast) = asts.get(j) {
				if let AST::Ident(l, ()) = ast.clone() {
					if let Some((b, op)) = ops
						.iter()
						.enumerate()
						.find(|(_, op)| op.name == l.to_chars().iter().collect::<String>())
					{
						if j < op.left || j + op.right >= asts.len() {
							Err(format!("not enough operator arguments for {}", l))?
						}
						asts.remove(j);
						let c: Vec<AST<()>> = asts.drain(j - op.left..j + op.right).collect();
						j -= op.left;
						asts.insert(j, AST::Operator((a, b), l, c, ()));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		Ok(asts)
	}
	parse(tokens, chars, &mut 0, None)
}

#[test]
fn parse_test() {
	let chars: Vec<char> = "{".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars),
		Err("no close bracket at 1:2-1:2".to_owned())
	);
	let chars: Vec<char> = ")".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars),
		Err("extra close bracket at 1:1-1:2".to_owned())
	);
	let chars: Vec<char> = "({)}".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars),
		Err("extra close bracket at 1:3-1:4".to_owned())
	);
	let chars: Vec<char> = "{}".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Brackets(
			Bracket::Curly,
			Location(0, 1, &chars),
			Location(1, 1, &chars),
			vec![],
			()
		)],
	);
	let chars: Vec<char> = "[{   }]".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Brackets(
			Bracket::Square,
			Location(0, 1, &chars),
			Location(6, 1, &chars),
			vec![AST::Brackets(
				Bracket::Curly,
				Location(1, 1, &chars),
				Location(5, 1, &chars),
				vec![],
				()
			)],
			(),
		)]
	);
	let chars: Vec<char> = "+ 1".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars),
		Err("not enough operator arguments for 1:1-1:2".to_owned())
	);
	let chars: Vec<char> = "1 ->".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars),
		Err("not enough operator arguments for 1:3-1:5".to_owned())
	);
	let chars: Vec<char> = "a + b".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Operator(
			(1, 0),
			Location(2, 1, &chars),
			vec![AST::Ident(Location(0, 1, &chars), ()), AST::Ident(Location(4, 1, &chars), ())],
			()
		)],
	);
	let chars: Vec<char> = "a - b * c".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Operator(
			(1, 1),
			Location(2, 1, &chars),
			vec![
				AST::Ident(Location(0, 1, &chars), ()),
				AST::Operator(
					(0, 0),
					Location(6, 1, &chars),
					vec![
						AST::Ident(Location(4, 1, &chars), ()),
						AST::Ident(Location(8, 1, &chars), ())
					],
					()
				),
			],
			(),
		)]
	);
	let chars: Vec<char> = "a *  b *  c".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Operator(
			(0, 0),
			Location(7, 1, &chars),
			vec![
				AST::Operator(
					(0, 0),
					Location(2, 1, &chars),
					vec![
						AST::Ident(Location(0, 1, &chars), ()),
						AST::Ident(Location(5, 1, &chars), ())
					],
					()
				),
				AST::Ident(Location(10, 1, &chars), ()),
			],
			(),
		)]
	);
	let chars: Vec<char> = "a -> b -> c".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Operator(
			(2, 0),
			Location(2, 2, &chars),
			vec![
				AST::Ident(Location(0, 1, &chars), ()),
				AST::Operator(
					(2, 0),
					Location(7, 2, &chars),
					vec![
						AST::Ident(Location(5, 1, &chars), ()),
						AST::Ident(Location(10, 1, &chars), ())
					],
					(),
				),
			],
			(),
		)]
	);
	let chars: Vec<char> = "[(a) + ({3 * 97})]".chars().collect::<Vec<char>>();
	assert_eq!(
		parse(&tokenize(&chars).unwrap(), &chars).unwrap(),
		vec![AST::Brackets(
			Bracket::Square,
			Location(0, 1, &chars),
			Location(17, 1, &chars),
			vec![AST::Operator(
				(1, 0),
				Location(5, 1, &chars),
				vec![
					AST::Brackets(
						Bracket::Round,
						Location(1, 1, &chars),
						Location(3, 1, &chars),
						vec![AST::Ident(Location(2, 1, &chars), ())],
						(),
					),
					AST::Brackets(
						Bracket::Round,
						Location(7, 1, &chars),
						Location(16, 1, &chars),
						vec![AST::Brackets(
							Bracket::Curly,
							Location(8, 1, &chars),
							Location(15, 1, &chars),
							vec![AST::Operator(
								(0, 0),
								Location(11, 1, &chars),
								vec![
									AST::Number(3, Location(9, 1, &chars), ()),
									AST::Number(97, Location(13, 2, &chars), ()),
								],
								(),
							)],
							(),
						)],
						(),
					),
				],
				(),
			)],
			(),
		)]
	);
}
