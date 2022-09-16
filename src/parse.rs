use crate::tokenize::Bracket;
use crate::tokenize::Location;
use crate::tokenize::Token;

pub struct Operator {
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
pub enum AST<'a> {
	Ident(Location<'a>),
	String(String, Location<'a>),
	Number(i64, Location<'a>),
	Brackets(Bracket, Location<'a>, Vec<AST<'a>>),
	Operator((usize, usize), Location<'a>, Vec<AST<'a>>),
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Result<Vec<AST<'a>>, String> {
	fn parse<'a>(
		tokens: &[Token<'a>],
		i: &mut usize,
		end: Option<Bracket>,
	) -> Result<Vec<AST<'a>>, String> {
		let mut asts = vec![];
		loop {
			asts.push(match (end, tokens.get(*i)) {
				(Some(_), None) => Err("missing close bracket".to_owned())?,
				(Some(end), Some(Token::Closer(b, _))) if *b == end => break,
				(None, None) => break,
				(_, Some(Token::Closer(_, l))) => {
					Err(format!("extra close bracket at {:?}", l))?
				}
				(_, Some(Token::Ident(l))) => AST::Ident(*l),
				(_, Some(Token::String(s, l))) => AST::String(s.clone(), *l),
				(_, Some(Token::Number(n, l))) => AST::Number(*n, *l),
				(_, Some(Token::Opener(b, l))) => {
					*i += 1;
					let asts = parse(tokens, i, Some(*b))?;
					match tokens[*i] {
						Token::Closer(_, k) => AST::Brackets(*b, Location(l.0, k.0 - l.0 + 1, l.2), asts),
						_ => unreachable!(),
					}
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
							Err(format!("not enough operator arguments for {:?}", l))?
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
	parse(tokens, &mut 0, None)
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
		vec![AST::Brackets(Bracket::Curly, Location(0, 2, &chars), vec![],)],
	);
	let chars: Vec<char> = "[{   }]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Brackets(
			Bracket::Square,
			Location(0, 7, &chars),
			vec![AST::Brackets(Bracket::Curly, Location(1, 5, &chars), vec![],)],
		)]
	);
	assert_eq!(
		parse(&tokenize(&"+ 1".chars().collect::<Vec<char>>()).unwrap()),
		Err("not enough operator arguments for 1:1-1:2".to_owned())
	);
	assert_eq!(
		parse(&tokenize(&"1 ->".chars().collect::<Vec<char>>()).unwrap()),
		Err("not enough operator arguments for 1:3-1:5".to_owned())
	);
	let chars: Vec<char> = "a + b".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Operator(
			(1, 0),
			Location(2, 1, &chars),
			vec![AST::Ident(Location(0, 1, &chars)), AST::Ident(Location(4, 1, &chars))],
		)],
	);
	let chars: Vec<char> = "a - b * c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Operator(
			(1, 1),
			Location(2, 1, &chars),
			vec![
				AST::Ident(Location(0, 1, &chars)),
				AST::Operator(
					(0, 0),
					Location(6, 1, &chars),
					vec![AST::Ident(Location(4, 1, &chars)), AST::Ident(Location(8, 1, &chars))],
				),
			],
		)]
	);
	let chars: Vec<char> = "a *  b *  c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Operator(
			(0, 0),
			Location(7, 1, &chars),
			vec![
				AST::Operator(
					(0, 0),
					Location(2, 1, &chars),
					vec![AST::Ident(Location(0, 1, &chars)), AST::Ident(Location(5, 1, &chars))],
				),
				AST::Ident(Location(10, 1, &chars)),
			],
		)]
	);
	let chars: Vec<char> = "a -> b -> c".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Operator(
			(2, 0),
			Location(2, 2, &chars),
			vec![
				AST::Ident(Location(0, 1, &chars)),
				AST::Operator(
					(2, 0),
					Location(7, 2, &chars),
					vec![
						AST::Ident(Location(5, 1, &chars)),
						AST::Ident(Location(10, 1, &chars))
					],
				),
			],
		)]
	);
	let chars: Vec<char> = "[(a) + ({3 * 97})]".chars().collect();
	assert_eq!(
		parse(&tokenize(&chars).unwrap()).unwrap(),
		vec![AST::Brackets(
			Bracket::Square,
			Location(0, 18, &chars),
			vec![AST::Operator(
				(1, 0),
				Location(5, 1, &chars),
				vec![
					AST::Brackets(
						Bracket::Round,
						Location(1, 3, &chars),
						vec![AST::Ident(Location(2, 1, &chars))],
					),
					AST::Brackets(
						Bracket::Round,
						Location(7, 10, &chars),
						vec![AST::Brackets(
							Bracket::Curly,
							Location(8, 8, &chars),
							vec![AST::Operator(
								(0, 0),
								Location(11, 1, &chars),
								vec![
									AST::Number(3, Location(9, 1, &chars)),
									AST::Number(97, Location(13, 2, &chars)),
								],
							)],
						)],
					),
				],
			)],
		)]
	);
}
