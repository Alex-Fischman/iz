fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let trees = rewriter(&trees);
	let trees = typer(&trees);

	fn print_trees(trees: &[Tree<Effect>], depth: usize) {
		for tree in trees {
			print!("{}", "\t".repeat(depth));
			match tree {
				Tree::Brackets(b, cs, e) => {
					println!("{}\t{:?}\t{:?}", b.to_char(Side::Open), e.inputs, e.outputs);
					print_trees(cs, depth + 1);
				}
				Tree::String(s, e) => println!("{:?}\t{:?}\t{:?}", s, e.inputs, e.outputs),
				Tree::Identifier(i, e) => println!("{}\t{:?}\t{:?}", i, e.inputs, e.outputs),
				Tree::Number(n, e) => println!("{}\t{:?}\t{:?}", n, e.inputs, e.outputs),
				Tree::Operator(i, cs, e) => {
					println!("{}\t{:?}\t{:?}", i, e.inputs, e.outputs);
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
			Number(1, ()),
			Brackets(Bracket::Round, vec![Number(2, ())], ()),
			Number(3, ()),
			Identifier("mul".to_string(), ()),
			Identifier("add".to_string(), ()),
			Identifier("a".to_string(), ()),
			Identifier("=".to_string(), ()),
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

//                   name     func     left   right  reverse
type Operator<'a> = (&'a str, &'a str, usize, usize, bool);
//                    operators  right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
	(&[("@", "nop", 1, 1, true)], false),
	(&[("-", "neg", 0, 1, false), ("not", "_not_", 0, 1, false)], true),
	(&[("*", "mul", 1, 1, false)], false),
	(&[("+", "add", 1, 1, false)], false),
	(
		&[
			("==", "eq", 1, 1, false),
			("!=", "ne", 1, 1, false),
			("<", "lt", 1, 1, false),
			(">", "gt", 1, 1, false),
			("<=", "le", 1, 1, false),
			(">=", "ge", 1, 1, false),
		],
		false,
	),
	(&[("and", "_and_", 1, 1, false), ("or", "_or_", 1, 1, false)], true),
	(&[("=", "=", 1, 1, true)], true),
	(&[("if", "_if_", 0, 2, true), ("while", "_while_", 0, 2, false)], true),
	(&[("else", "_else_", 1, 1, true)], true),
];

#[derive(Clone, Debug, PartialEq)]
enum Tree<Tag> {
	Brackets(Bracket, Vec<Tree<Tag>>, Tag),
	String(String, Tag),
	Identifier(String, Tag),
	Number(i64, Tag),
	Operator(String, Vec<Tree<Tag>>, Tag),
}

impl<Tag> Tree<Tag> {
	fn get_tag(&self) -> &Tag {
		match self {
			Tree::Brackets(.., tag) => tag,
			Tree::String(.., tag) => tag,
			Tree::Identifier(.., tag) => tag,
			Tree::Number(.., tag) => tag,
			Tree::Operator(.., tag) => tag,
		}
	}
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
				if operator.4 {
					cs.reverse();
				}
				let cs = rewriter(&cs);
				trees.insert(j + 1, Tree::Identifier(operator.1.to_owned(), ()));
				let out = cs.len() + 1;
				trees.splice(j..j + 1, cs);
				out
			}
			_ => 1,
		}
	}
	trees
}

#[derive(Clone, Debug)]
enum Type {
	Data(String),
	Block(Effect),
	Unknown,
	SameAs(usize), // index into inputs
}

impl Type {
	fn int() -> Type {
		Type::Data("int".to_string())
	}

	fn bool() -> Type {
		Type::Data("bool".to_string())
	}

	fn string() -> Type {
		Type::Data("string".to_string())
	}
}

#[derive(Clone, Debug)]
struct Effect {
	inputs: Vec<Type>,
	outputs: Vec<Type>,
}

impl Effect {
	fn literal(t: Type) -> Effect {
		Effect { inputs: vec![], outputs: vec![t] }
	}

	fn equalize(a: &mut Type, b: &Type) -> bool {
		match (a, b) {
			(Type::Data(a), Type::Data(b)) => a == b,
			_ => todo!(),
		}
	}

	fn compose(&mut self, other: &Effect) {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		self.outputs
			.drain(i..)
			.zip(&other.inputs[j..])
			.all(|(mut a, b)| Effect::equalize(&mut a, b));
		self.inputs.extend(other.inputs[..j].iter().cloned());
		self.outputs.extend(other.outputs.iter().cloned());
	}
}

fn typer(trees: &[Tree<()>]) -> Vec<Tree<Effect>> {
	trees
		.iter()
		.map(|tree| match tree {
			Tree::Brackets(b, cs, ()) => {
				let cs = typer(cs);
				let mut t = Effect { inputs: vec![], outputs: vec![] };
				cs.iter().for_each(|c| t.compose(c.get_tag()));
				let t = match b {
					Bracket::Round => t,
					Bracket::Curly => Effect::literal(Type::Block(t)),
					Bracket::Square => todo!("arrays (type variables)"),
				};
				Tree::Brackets(*b, cs, t)
			}
			Tree::String(s, ()) => Tree::String(s.clone(), Effect::literal(Type::string())),
			Tree::Identifier(i, ()) => Tree::Identifier(
				i.clone(),
				match i.as_str() {
					"false" | "true" => Effect::literal(Type::bool()),
					"nop" => Effect { inputs: vec![], outputs: vec![] },
					"neg" => Effect { inputs: vec![Type::int()], outputs: vec![Type::int()] },
					"_not_" => {
						Effect { inputs: vec![Type::bool()], outputs: vec![Type::bool()] }
					}
					"mul" | "add" => Effect {
						inputs: vec![Type::int(), Type::int()],
						outputs: vec![Type::int()],
					},
					"eq" | "ne" => Effect {
						inputs: vec![Type::Unknown, Type::SameAs(0)],
						outputs: vec![Type::bool()],
					},
					"lt" | "gt" | "le" | "ge" => Effect {
						inputs: vec![Type::int(), Type::int()],
						outputs: vec![Type::bool()],
					},
					"_and_" | "_or_" => Effect {
						inputs: vec![Type::bool(), Type::bool()],
						outputs: vec![Type::bool()],
					},
					"=" => todo!("value variables"),
					"_if_" | "_else_" => todo!("optionals"),
					"_while_" => Effect {
						inputs: vec![
							Type::Block(Effect::literal(Type::bool())),
							Type::Block(Effect { inputs: vec![], outputs: vec![] }),
						],
						outputs: vec![],
					},
					i => todo!("{}", i),
				},
			),
			Tree::Number(n, ()) => Tree::Number(*n, Effect::literal(Type::int())),
			Tree::Operator(..) => unreachable!(),
		})
		.collect()
}
