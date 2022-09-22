use crate::tokenize::Bracket;
use crate::tokenize::Token;
use crate::Error;
use crate::Location;

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
pub struct Tree {
	pub data: Parsed,
	pub location: Location,
	pub children: Vec<Tree>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Parsed {
	Name(String),
	String(String),
	Number(i64),
	Brackets(Bracket),
}

pub fn ident(i: &str, location: Location) -> Tree {
	Tree { data: Parsed::Name(i.to_owned()), location, children: vec![] }
}
pub fn string(s: String, location: Location) -> Tree {
	Tree { data: Parsed::String(s), location, children: vec![] }
}
pub fn number(n: i64, location: Location) -> Tree {
	Tree { data: Parsed::Number(n), location, children: vec![] }
}
pub fn brackets(b: Bracket, location: Location, children: Vec<Tree>) -> Tree {
	Tree { data: Parsed::Brackets(b), location, children }
}
pub fn operator(i: &str, location: Location, children: Vec<Tree>) -> Tree {
	Tree { data: Parsed::Name(i.to_owned()), location, children }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Tree>, Error> {
	fn parse(
		tokens: &[Token],
		i: &mut usize,
		end: Option<Bracket>,
	) -> Result<Vec<Tree>, Error> {
		let mut asts = vec![];
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => Err(Error::new("missing close bracket", Location(0, 0)))?,
				(Some(end), Some(Token::Closer(b, _))) if *b == end => break,
				(None, None) => break,
				(_, Some(Token::Closer(_, l))) => Err(Error::new("extra close bracket", *l))?,
				(_, Some(Token::Ident(s, l))) => ident(s, *l),
				(_, Some(Token::String(s, l))) => string(s.clone(), *l),
				(_, Some(Token::Number(n, l))) => number(*n, *l),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let asts = parse(tokens, i, Some(*b))?;
					match tokens[*i] {
						Token::Closer(_, k) => brackets(*b, Location(l.0, k.0 - l.0 + 1), asts),
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
					if let Some(op) = ops.iter().find(|op| op.0 == i) {
						if j < op.2 || j + op.3 >= asts.len() {
							Err(Error::new("not enough operator arguments", l))?
						}
						asts.remove(j);
						let c: Vec<Tree> = asts.drain(j - op.2..j + op.3).collect();
						j -= op.2;
						asts.insert(j, operator(op.1, l, c));
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
		Err(Error::new("missing close bracket", Location(0, 0)))
	);
	assert_eq!(
		parse(&tokenize(&")".chars().collect::<Vec<char>>()).unwrap()),
		Err(Error::new("extra close bracket", Location(0, 1)))
	);
	assert_eq!(
		parse(&tokenize(&"({)}".chars().collect::<Vec<char>>()).unwrap()),
		Err(Error::new("extra close bracket", Location(2, 1)))
	);
	let chars: Vec<char> = "{}".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(Bracket::Curly, Location(0, 2), vec![])],
	);
	let chars: Vec<char> = "[{   }]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(
			Bracket::Square,
			Location(0, 7),
			vec![brackets(Bracket::Curly, Location(1, 5), vec![],)],
		)]
	);
	assert_eq!(
		parse(&tokenize(&"+ 1".chars().collect::<Vec<char>>()).unwrap()),
		Err(Error::new("not enough operator arguments", Location(0, 1)))
	);
	assert_eq!(
		parse(&tokenize(&"1 =".chars().collect::<Vec<char>>()).unwrap()),
		Err(Error::new("not enough operator arguments", Location(2, 1)))
	);
	let chars: Vec<char> = "a + b".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"add",
			Location(2, 1),
			vec![ident("a", Location(0, 1)), ident("b", Location(4, 1))],
		)],
	);
	let chars: Vec<char> = "a - b * c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"sub",
			Location(2, 1),
			vec![
				ident("a", Location(0, 1)),
				operator(
					"mul",
					Location(6, 1),
					vec![ident("b", Location(4, 1)), ident("c", Location(8, 1))],
				),
			],
		)]
	);
	let chars: Vec<char> = "a * b * c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"mul",
			Location(6, 1),
			vec![
				operator(
					"mul",
					Location(2, 1),
					vec![ident("a", Location(0, 1)), ident("b", Location(4, 1))],
				),
				ident("c", Location(8, 1)),
			],
		)]
	);
	let chars: Vec<char> = "a = b = c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![operator(
			"assign",
			Location(2, 1),
			vec![
				ident("a", Location(0, 1)),
				operator(
					"assign",
					Location(6, 1),
					vec![ident("b", Location(4, 1)), ident("c", Location(8, 1))],
				),
			],
		)]
	);
	let chars: Vec<char> = "[(a) + ({3 * 97})]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![brackets(
			Bracket::Square,
			Location(0, 18),
			vec![operator(
				"add",
				Location(5, 1),
				vec![
					brackets(Bracket::Round, Location(1, 3), vec![ident("a", Location(2, 1))],),
					brackets(
						Bracket::Round,
						Location(7, 10),
						vec![brackets(
							Bracket::Curly,
							Location(8, 8),
							vec![operator(
								"mul",
								Location(11, 1),
								vec![number(3, Location(9, 1)), number(97, Location(13, 2)),],
							)],
						)],
					),
				],
			)],
		)]
	);
}
