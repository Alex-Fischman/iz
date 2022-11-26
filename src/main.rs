fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let trees = rewriter(&trees);

	fn print_trees(trees: &[Tree<()>], depth: usize) {
		for tree in trees {
			print!("{}", "\t".repeat(depth));
			match tree {
				Tree::Brackets(b, cs, ()) => {
					println!("{}", b.to_char(Side::Open));
					print_trees(cs, depth + 1);
				}
				Tree::String(s, ()) => println!("{:?}", s),
				Tree::Identifier(i, ()) => println!("{}", i),
				Tree::Number(n, ()) => println!("{}", n),
				Tree::Operator(i, cs, ()) => {
					println!("{}", i);
					print_trees(cs, depth + 1);
				}
			}
		}
	}
	print_trees(&trees, 0);
}

#[test]
fn test() {
	use Tree::*;
	assert_eq!(
		tokenizer(&"1+2 0b10_0000 0xff \"a b\tc\nd\\\"\"".chars().collect::<Vec<char>>()),
		vec![
			Token::Number(1),
			Token::Identifier("+".to_string()),
			Token::Number(2),
			Token::Number(32),
			Token::Number(255),
			Token::String("a b\tc\nd\"".to_string()),
		]
	);
	let parse_test = "if false 1 else if true 2 else 3\n# asdf\na = 1 + (2) * 3";
	let parse_test = parser(&tokenizer(&parse_test.chars().collect::<Vec<char>>()));
	assert_eq!(
		parse_test,
		vec![
			Operator(
				"else".to_string(),
				vec![
					Operator(
						"if".to_string(),
						vec![Identifier("false".to_string(), ()), Number(1, ())],
						()
					),
					Operator(
						"else".to_string(),
						vec![
							Operator(
								"if".to_string(),
								vec![Identifier("true".to_string(), ()), Number(2, ())],
								()
							),
							Number(3, ()),
						],
						()
					),
				],
				()
			),
			Operator(
				"=".to_string(),
				vec![
					Identifier("a".to_string(), ()),
					Operator(
						"+".to_string(),
						vec![
							Number(1, ()),
							Operator(
								"*".to_string(),
								vec![
									Brackets(Bracket::Round, vec![Number(2, ())], ()),
									Number(3, ())
								],
								()
							)
						],
						()
					)
				],
				()
			),
		]
	);
	assert_eq!(
		rewriter(&parse_test),
		vec![
			Number(3, ()),
			Number(2, ()),
			Identifier("true".to_string(), ()),
			Identifier("_if_".to_string(), ()),
			Identifier("_else_".to_string(), ()),
			Number(1, ()),
			Identifier("false".to_string(), ()),
			Identifier("_if_".to_string(), ()),
			Identifier("_else_".to_string(), ()),
			Operator(
				"=".to_string(),
				vec![
					Identifier("a".to_string(), ()),
					Number(1, ()),
					Brackets(Bracket::Round, vec![Number(2, ())], ()),
					Number(3, ()),
					Identifier("mul".to_string(), ()),
					Identifier("add".to_string(), ()),
				],
				()
			),
		]
	);
}

#[derive(Debug, PartialEq)]
enum Token {
	Bracket(Bracket, Side),
	String(String),
	Identifier(String),
	Number(i64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Bracket {
	Round,
	Curly,
	Square,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Side {
	Open,
	Close,
}

impl Bracket {
	fn to_char(self, side: Side) -> char {
		match (self, side) {
			(Bracket::Round, Side::Open) => '(',
			(Bracket::Round, Side::Close) => ')',
			(Bracket::Curly, Side::Open) => '{',
			(Bracket::Curly, Side::Close) => '}',
			(Bracket::Square, Side::Open) => '[',
			(Bracket::Square, Side::Close) => ']',
		}
	}
}

fn tokenizer(chars: &[char]) -> Vec<Token> {
	let mut tokens: Vec<Token> = Vec::new();
	let mut i = 0;
	while i < chars.len() {
		match chars[i] {
			'#' => {
				while i < chars.len() && chars[i] != '\n' {
					i += 1;
				}
			}
			' ' | '\t' | '\n' | '\r' => {}
			'(' => tokens.push(Token::Bracket(Bracket::Round, Side::Open)),
			')' => tokens.push(Token::Bracket(Bracket::Round, Side::Close)),
			'{' => tokens.push(Token::Bracket(Bracket::Curly, Side::Open)),
			'}' => tokens.push(Token::Bracket(Bracket::Curly, Side::Close)),
			'[' => tokens.push(Token::Bracket(Bracket::Square, Side::Open)),
			']' => tokens.push(Token::Bracket(Bracket::Square, Side::Close)),
			'"' => {
				i += 1;
				let mut string = String::new();
				loop {
					if i >= chars.len() {
						panic!("missing end quote");
					} else if chars[i] == '\\' {
						i += 1;
						string.push(match chars.get(i) {
							Some('\\') => '\\',
							Some('t') => '\t',
							Some('n') => '\n',
							Some('r') => '\r',
							Some('"') => '"',
							Some(c) => panic!("invalid escape character {}", c),
							None => panic!("missing escape character"),
						});
					} else if chars[i] == '"' {
						break;
					} else {
						string.push(chars[i]);
					}
					i += 1;
				}
				tokens.push(Token::String(string));
			}
			c => {
				let start = i;
				let is_alphanumeric =
					|c: char| matches!(c, '_' | 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9');
				while i < chars.len()
					&& !"\"#(){}{} \t\n".contains(chars[i])
					&& is_alphanumeric(c) == is_alphanumeric(chars[i])
				{
					i += 1;
				}

				tokens.push(Token::Identifier(chars[start..i].iter().collect()));

				i -= 1;
			}
		}

		i += 1;
	}

	for token in &mut tokens {
		if let Token::Identifier(i) = token {
			let chars: Vec<char> = i.chars().collect();
			if '0' <= chars[0] && chars[0] <= '9' {
				if chars.len() == 1 {
					*token = Token::Number(chars[0] as i64 - '0' as i64);
				} else {
					let mut value = 0;
					let (base, start) = match (chars[0], chars[1]) {
						('0', 'x') => (16, 2),
						('0', 'b') => (2, 2),
						(_, _) => (10, 0),
					};
					if start >= chars.len() {
						panic!("no digits after base specifier");
					}
					for c in chars.iter().skip(start) {
						if *c != '_' {
							let digit = match *c {
								'0'..='9' => *c as i64 - '0' as i64,
								'A'..='F' => *c as i64 - 'A' as i64 + 10,
								'a'..='f' => *c as i64 - 'a' as i64 + 10,
								d => panic!("invalid digit {}", d),
							};
							if digit >= base {
								panic!("invalid digit {} in base {}", digit, base);
							}
							value = value * base + digit;
						}
					}
					*token = Token::Number(value);
				}
			}
		}
	}

	tokens
}

//                   name     func     left   right  unwrap
type Operator<'a> = (&'a str, &'a str, usize, usize, Rewrite);
#[derive(Debug, PartialEq)]
enum Rewrite {
	Unwrap,
	UnwrapReverse,
	NoUnwrap,
}
//                    operators  right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
	(&[("@", "nop", 1, 1, Rewrite::UnwrapReverse)], false),
	(&[("-", "neg", 0, 1, Rewrite::Unwrap), ("not", "_not_", 0, 1, Rewrite::Unwrap)], true),
	(&[("*", "mul", 1, 1, Rewrite::Unwrap)], false),
	(&[("+", "add", 1, 1, Rewrite::Unwrap)], false),
	(
		&[
			("==", "eq", 1, 1, Rewrite::Unwrap),
			("!=", "ne", 1, 1, Rewrite::Unwrap),
			("<", "lt", 1, 1, Rewrite::Unwrap),
			(">", "gt", 1, 1, Rewrite::Unwrap),
			("<=", "le", 1, 1, Rewrite::Unwrap),
			(">=", "ge", 1, 1, Rewrite::Unwrap),
		],
		false,
	),
	(&[("and", "_and_", 1, 1, Rewrite::Unwrap), ("or", "_or_", 1, 1, Rewrite::Unwrap)], true),
	(&[("=", "=", 1, 1, Rewrite::NoUnwrap)], true),
	(
		&[
			("if", "_if_", 0, 2, Rewrite::UnwrapReverse),
			("while", "_while_", 0, 2, Rewrite::Unwrap),
		],
		true,
	),
	(&[("else", "_else_", 1, 1, Rewrite::UnwrapReverse)], true),
];

#[derive(Clone, Debug, PartialEq)]
enum Tree<Tag> {
	Brackets(Bracket, Vec<Tree<Tag>>, Tag),
	String(String, Tag),
	Identifier(String, Tag),
	Number(i64, Tag),
	Operator(String, Vec<Tree<Tag>>, Tag),
}

fn parser(tokens: &[Token]) -> Vec<Tree<()>> {
	fn parse(tokens: &[Token], i: &mut usize, mut searching: Option<Bracket>) -> Vec<Tree<()>> {
		let mut trees = Vec::new();
		while *i < tokens.len() {
			let token = &tokens[*i];
			*i += 1;
			trees.push(match token {
				Token::Bracket(b, Side::Open) => {
					Tree::Brackets(*b, parse(tokens, i, Some(*b)), ())
				}
				Token::Bracket(b, Side::Close) => match (b, searching) {
					(b, Some(c)) if *b == c => {
						searching = None;
						break;
					}
					(b, _) => panic!("extra {}", b.to_char(Side::Close)),
				},
				Token::String(s) => Tree::String(s.clone(), ()),
				Token::Identifier(i) => Tree::Identifier(i.clone(), ()),
				Token::Number(n) => Tree::Number(*n, ()),
			});
		}
		if let Some(b) = searching {
			panic!("extra {}", b.to_char(Side::Open));
		}

		for (operators, right) in OPERATORS {
			let mut j = if *right { trees.len().wrapping_sub(1) } else { 0 };
			while let Some(tree) = trees.get(j) {
				if let Tree::Identifier(i, ()) = tree {
					let i = i.clone();
					if let Some(operator) = operators.iter().find(|op| op.0 == i) {
						if j < operator.2 || j + operator.3 >= trees.len() {
							panic!("not enough operator arguments for {}", i);
						}
						trees.remove(j);
						let cs: Vec<Tree<()>> =
							trees.drain(j - operator.2..j + operator.3).collect();
						j -= operator.2;
						trees.insert(j, Tree::Operator(i, cs, ()));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}

		trees
	}

	parse(tokens, &mut 0, None)
}

fn rewriter(trees: &[Tree<()>]) -> Vec<Tree<()>> {
	let mut trees = Vec::from(trees);
	let mut j = 0;
	while j < trees.len() {
		j += match trees[j].clone() {
			Tree::Brackets(b, cs, ()) => {
				trees[j] = Tree::Brackets(b, rewriter(&cs), ());
				1
			}
			Tree::Operator(i, cs, ()) => {
				let operator = OPERATORS
					.iter()
					.find_map(|(ops, _)| ops.iter().find(|op| op.0 == i))
					.unwrap();
				let mut cs = cs.clone();
				if operator.4 == Rewrite::UnwrapReverse {
					cs.reverse();
				}
				let cs = rewriter(&cs);
				if operator.4 == Rewrite::NoUnwrap {
					trees.splice(j..j + 1, [Tree::Operator(operator.1.to_owned(), cs, ())]);
					1
				} else {
					trees.insert(j + 1, Tree::Identifier(operator.1.to_owned(), ()));
					let out = cs.len() + 1;
					trees.splice(j..j + 1, cs);
					out
				}
			}
			_ => 1,
		}
	}
	trees
}
