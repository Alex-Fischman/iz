use crate::tokenize::Bracket;
use crate::tokenize::Location;
use crate::tokenize::Token;

type Operator<'a> = (&'a str, &'a str, usize, usize);
// Grouped by precedence; highest first
// (&[name, func, left args, right args], (right assoc))
pub const OPERATORS: &[(&[Operator], bool)] = &[
	(&[("@", "swap_call", 1, 1)], false),
	(&[("!", "not", 0, 1)], true),
	(&[("*", "mul", 1, 1)], false),
	(&[("+", "add", 1, 1), ("-", "sub", 1, 1)], false),
	(
		&[
			("==", "eq", 1, 1),
			("!=", "ne", 1, 1),
			("<", "lt", 1, 1),
			(">", "gt", 1, 1),
			("<=", "le", 1, 1),
			(">=", "ge", 1, 1),
		],
		false,
	),
	(&[("=", "assign", 1, 1)], true),
	(&[("if", "_if_", 0, 2)], true),
	(&[("else", "_else_", 1, 1)], true),
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tree<'a> {
	pub data: Parsed,
	pub location: Location<'a>,
	pub children: Vec<Tree<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Parsed {
	Name(String),
	String(String),
	Number(i64),
	Brackets(Bracket),
}

pub fn ident<'a>(i: &str, location: Location<'a>) -> Tree<'a> {
	Tree { data: Parsed::Name(i.to_owned()), location, children: vec![] }
}
pub fn string(s: String, location: Location) -> Tree {
	Tree { data: Parsed::String(s), location, children: vec![] }
}
pub fn number(n: i64, location: Location) -> Tree {
	Tree { data: Parsed::Number(n), location, children: vec![] }
}
pub fn brackets<'a>(b: Bracket, location: Location<'a>, children: Vec<Tree<'a>>) -> Tree<'a> {
	Tree { data: Parsed::Brackets(b), location, children }
}
pub fn operator<'a>(i: &str, location: Location<'a>, children: Vec<Tree<'a>>) -> Tree<'a> {
	Tree { data: Parsed::Name(i.to_owned()), location, children }
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Vec<Tree<'a>>, String> {
	fn parse<'a>(
		tokens: &[Token<'a>],
		i: &mut usize,
		end: Option<Bracket>,
	) -> Result<Vec<Tree<'a>>, String> {
		let mut asts = vec![];
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => Err("missing close bracket".to_owned())?,
				(Some(end), Some(Token::Closer(b, _))) if *b == end => break,
				(None, None) => break,
				(_, Some(Token::Closer(_, l))) => {
					Err(format!("extra close bracket at {:?}", l))?
				}
				(_, Some(Token::Ident(l))) => {
					let name = l.to_chars().iter().collect::<String>();
					OPERATORS
						.iter()
						.flat_map(|os| os.0)
						.find_map(|o| if o.0 == name { Some(ident(o.1, *l)) } else { None })
						.unwrap_or_else(|| ident(&name, *l))
				}
				(_, Some(Token::String(s, l))) => string(s.clone(), *l),
				(_, Some(Token::Number(n, l))) => number(*n, *l),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let asts = parse(tokens, i, Some(*b))?;
					match tokens[*i] {
						Token::Closer(_, k) => {
							brackets(*b, Location(l.0, k.0 - l.0 + 1, l.2), asts)
						}
						_ => unreachable!(),
					}
				}
			});
			*i += 1;
		}
		for (ops, right) in OPERATORS.iter() {
			let mut j = if *right { asts.len().wrapping_sub(1) } else { 0 };
			while let Some(ast) = asts.get(j) {
				if let Parsed::Name(i) = ast.data.clone() {
					let l = ast.location;
					if let Some(op) =
						ops.iter().find(|op| op.0 == l.to_chars().iter().collect::<String>())
					{
						if j < op.2 || j + op.3 >= asts.len() {
							Err(format!("not enough operator arguments for {:?}", l))?
						}
						asts.remove(j);
						let c: Vec<Tree> = asts.drain(j - op.2..j + op.3).collect();
						j -= op.2;
						asts.insert(j, operator(&i, l, c));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		Ok(asts)
	}
	let trees = parse(tokens, &mut 0, None)?;
	Ok(trees)
}

#[test]
fn parse_test() {
	use crate::tokenize::tokenize;
	assert_eq!(
		parse(&tokenize(&"{".chars().collect::<Vec<char>>()).unwrap()),
		Err("missing close bracket".to_owned())
	);
	assert_eq!(
		parse(&tokenize(&")".chars().collect::<Vec<char>>()).unwrap()),
		Err("extra close bracket at 1:1-1:2".to_owned())
	);
	assert_eq!(
		parse(&tokenize(&"({)}".chars().collect::<Vec<char>>()).unwrap()),
		Err("extra close bracket at 1:3-1:4".to_owned())
	);
	let chars: Vec<char> = "{}".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(Bracket::Curly, Location(0, 2, &chars), vec![])],
	);
	let chars: Vec<char> = "[{   }]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(
			Bracket::Square,
			Location(0, 7, &chars),
			vec![brackets(Bracket::Curly, Location(1, 5, &chars), vec![],)],
		)]
	);
	assert_eq!(
		parse(&tokenize(&"+ 1".chars().collect::<Vec<char>>()).unwrap()),
		Err("not enough operator arguments for 1:1-1:2".to_owned())
	);
	assert_eq!(
		parse(&tokenize(&"1 =".chars().collect::<Vec<char>>()).unwrap()),
		Err("not enough operator arguments for 1:3-1:4".to_owned())
	);
	let chars: Vec<char> = "a + b".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"add",
			Location(2, 1, &chars),
			vec![ident("a", Location(0, 1, &chars)), ident("b", Location(4, 1, &chars))],
		)],
	);
	let chars: Vec<char> = "a - b * c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"sub",
			Location(2, 1, &chars),
			vec![
				ident("a", Location(0, 1, &chars)),
				operator(
					"mul",
					Location(6, 1, &chars),
					vec![ident("b", Location(4, 1, &chars)), ident("c", Location(8, 1, &chars))],
				),
			],
		)]
	);
	let chars: Vec<char> = "a * b * c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"mul",
			Location(6, 1, &chars),
			vec![
				operator(
					"mul",
					Location(2, 1, &chars),
					vec![ident("a", Location(0, 1, &chars)), ident("b", Location(4, 1, &chars))],
				),
				ident("c", Location(8, 1, &chars)),
			],
		)]
	);
	let chars: Vec<char> = "a = b = c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"assign",
			Location(2, 1, &chars),
			vec![
				ident("a", Location(0, 1, &chars)),
				operator(
					"assign",
					Location(6, 1, &chars),
					vec![ident("b", Location(4, 1, &chars)), ident("c", Location(8, 1, &chars))],
				),
			],
		)]
	);
	let chars: Vec<char> = "[(a) + ({3 * 97})]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(
			Bracket::Square,
			Location(0, 18, &chars),
			vec![operator(
				"add",
				Location(5, 1, &chars),
				vec![
					brackets(
						Bracket::Round,
						Location(1, 3, &chars),
						vec![ident("a", Location(2, 1, &chars))],
					),
					brackets(
						Bracket::Round,
						Location(7, 10, &chars),
						vec![brackets(
							Bracket::Curly,
							Location(8, 8, &chars),
							vec![operator(
								"mul",
								Location(11, 1, &chars),
								vec![
									number(3, Location(9, 1, &chars)),
									number(97, Location(13, 2, &chars)),
								],
							)],
						)],
					),
				],
			)],
		)]
	);
}
