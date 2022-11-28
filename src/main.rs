fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let trees = rewriter(&trees);
	let tree = Tree::Brackets(Bracket::Curly, trees, ());
	let tree = typer(&tree);
	let code = compile(&tree);

	println!("{:?}", code);
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
			Number(3, ()),
			Brackets(Bracket::Round, vec![Number(2, ())], ()),
			Identifier("mul".to_string(), ()),
			Number(1, ()),
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

//                   name     func     left   right
type Operator<'a> = (&'a str, &'a str, usize, usize);
//                    operators  right associativity
const OPERATORS: &[(&[Operator], bool)] = &[
	(&[("@", "nop", 1, 1)], false),
	(&[("-", "neg", 0, 1), ("not", "_not_", 0, 1)], true),
	(&[("*", "mul", 1, 1)], false),
	(&[("+", "add", 1, 1)], false),
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
	(&[("and", "_and_", 1, 1), ("or", "_or_", 1, 1)], true),
	(&[("=", "=", 1, 1)], true),
	(&[("if", "_if_", 0, 2), ("while", "_while_", 0, 2)], true),
	(&[("else", "_else_", 1, 1)], true),
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
	fn get_tag(&mut self) -> &mut Tag {
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
				cs.reverse();
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

#[derive(Clone)]
enum Type {
	Int,
	Bool,
	String,
	Block(Effect),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Int => write!(f, "int"),
			Type::Bool => write!(f, "bool"),
			Type::String => write!(f, "string"),
			Type::Block(e) => write!(f, "{:?}", e),
		}
	}
}

impl Type {
	fn equalize(a: &mut Type, b: &mut Type) -> bool {
		match (a, b) {
			(Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::String, Type::String) => {
				true
			}
			(Type::Block(a), Type::Block(b)) => {
				a.inputs.iter_mut().zip(&mut b.inputs).all(|(a, b)| Type::equalize(a, b))
					&& a.outputs
						.iter_mut()
						.zip(&mut b.outputs)
						.all(|(a, b)| Type::equalize(a, b))
			}
			_ => false,
		}
	}
}

#[derive(Clone)]
struct Effect {
	inputs: Vec<Type>,
	outputs: Vec<Type>,
}

impl std::fmt::Debug for Effect {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{:?}->{:?}", self.inputs, self.outputs)
	}
}

impl Effect {
	fn literal(t: Type) -> Effect {
		Effect { inputs: vec![], outputs: vec![t] }
	}

	fn function(inputs: Vec<Type>, outputs: Vec<Type>) -> Effect {
		Effect { inputs, outputs }
	}

	fn compose(&mut self, other: &mut Effect) {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		assert!(self
			.outputs
			.drain(i..)
			.zip(&mut other.inputs[j..])
			.all(|(mut a, b)| Type::equalize(&mut a, b)));
		self.inputs.extend(other.inputs[..j].iter().cloned());
		self.outputs.extend(other.outputs.iter().cloned());
	}
}

fn typer(tree: &Tree<()>) -> Tree<Effect> {
	match tree {
		Tree::Brackets(b, cs, ()) => {
			let mut cs: Vec<Tree<Effect>> = cs.iter().map(typer).collect();
			let mut t = Effect::function(vec![], vec![]);
			cs.iter_mut().for_each(|c| t.compose(c.get_tag()));
			let t = match b {
				Bracket::Round => t,
				Bracket::Curly => Effect::literal(Type::Block(t)),
				Bracket::Square => todo!(),
			};
			Tree::Brackets(*b, cs, t)
		}
		Tree::String(s, ()) => Tree::String(s.clone(), Effect::literal(Type::String)),
		Tree::Identifier(i, ()) => Tree::Identifier(
			i.clone(),
			match i.as_str() {
				"false" | "true" => Effect::literal(Type::Bool),
				"nop" => Effect::function(vec![], vec![]),
				"neg" => Effect::function(vec![Type::Int], vec![Type::Int]),
				"_not_" => Effect::function(vec![Type::Bool], vec![Type::Bool]),
				"mul" | "add" => Effect::function(vec![Type::Int, Type::Int], vec![Type::Int]),
				"eq" | "ne" => todo!("type constraints"),
				"lt" | "gt" | "le" | "ge" => {
					Effect::function(vec![Type::Int, Type::Int], vec![Type::Bool])
				}
				"_and_" | "_or_" => {
					Effect::function(vec![Type::Bool, Type::Bool], vec![Type::Bool])
				}
				"=" => todo!("value variables"),
				"_if_" | "_else_" => todo!("optionals"),
				"_while_" => Effect::function(
					vec![
						Type::Block(Effect::literal(Type::Bool)),
						Type::Block(Effect::function(vec![], vec![])),
					],
					vec![],
				),
				i => todo!("{}", i),
			},
		),
		Tree::Number(n, ()) => Tree::Number(*n, Effect::literal(Type::Int)),
		Tree::Operator(..) => unreachable!(),
	}
}

#[derive(Debug)]
enum Operation {
	IntLit(i64),
	IntNeg,
	IntMul,
	IntAdd,
	IntEq,
	IntLt,
	IntGt,
	BitNot,
	BitAnd,
	BitOr,
}

fn compile(tree: &Tree<Effect>) -> Vec<Operation> {
	use Operation::*;
	match tree {
		Tree::Brackets(Bracket::Round, cs, _) => cs.iter().flat_map(compile).collect(),
		// todo: labels
		Tree::Brackets(Bracket::Curly, cs, _) => cs.iter().flat_map(compile).collect(),
		Tree::Brackets(Bracket::Square, ..) => todo!(),
		Tree::String(..) => todo!(),
		Tree::Identifier(i, Effect { inputs, outputs }) => {
			match (i.as_str(), inputs.as_slice(), outputs.as_slice()) {
				("false", [], [Type::Bool]) => vec![IntLit(0)],
				("true", [], [Type::Bool]) => vec![IntLit(1)],
				("neg", [Type::Int], [Type::Int]) => vec![IntNeg],
				("not", [Type::Bool], [Type::Bool]) => vec![BitNot],
				("mul", [Type::Int, Type::Int], [Type::Int]) => vec![IntMul],
				("add", [Type::Int, Type::Int], [Type::Int]) => vec![IntAdd],
				("eq", [Type::Int, Type::Int], [Type::Bool])
				| ("eq", [Type::Bool, Type::Bool], [Type::Bool]) => vec![IntEq],
				("ne", [Type::Int, Type::Int], [Type::Bool])
				| ("ne", [Type::Bool, Type::Bool], [Type::Bool]) => vec![IntEq, BitNot],
				("lt", [Type::Int, Type::Int], [Type::Bool]) => vec![IntLt],
				("gt", [Type::Int, Type::Int], [Type::Bool]) => vec![IntGt],
				("le", [Type::Int, Type::Int], [Type::Bool]) => vec![IntGt, BitNot],
				("ge", [Type::Int, Type::Int], [Type::Bool]) => vec![IntLt, BitNot],
				("_and_", [Type::Bool, Type::Bool], [Type::Bool]) => vec![BitAnd],
				("_or_", [Type::Bool, Type::Bool], [Type::Bool]) => vec![BitOr],
				_ => todo!(),
			}
		}
		Tree::Number(n, _) => vec![IntLit(*n)],
		Tree::Operator(..) => unreachable!(),
	}
}
