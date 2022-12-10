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
	let mut code = compile(&tree);
	code.extend([Push(-1), Shove(1), Goto, Label(-1)]); // since there's no main yet, call the returned block
	println!("{:?}", code);
	let stack = run(&code);
	println!("{:?}", stack);
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
	let typer_test = tokenizer(&"{{1 2} call add call} call".chars().collect::<Vec<char>>());
	let typer_test = Tree::Brackets(Bracket::Curly, rewriter(&parser(&typer_test)), ());
	assert_eq!(
		typer(&typer_test),
		Tree::Brackets(
			Bracket::Curly,
			vec![
				Tree::Brackets(
					Bracket::Curly,
					vec![
						Tree::Brackets(
							Bracket::Curly,
							vec![
								Tree::Number(1, Effect::literal(Type::Int)),
								Tree::Number(2, Effect::literal(Type::Int)),
							],
							Effect::literal(Type::Block(Effect::function(
								vec![],
								vec![Type::Int, Type::Int]
							)))
						),
						Tree::Identifier(
							"call".to_string(),
							Effect::function(
								vec![Type::Block(Effect::function(
									vec![],
									vec![Type::Int, Type::Int]
								))],
								vec![Type::Int, Type::Int]
							)
						),
						Tree::Identifier(
							"add".to_string(),
							Effect::literal(Type::Block(Effect::function(
								vec![Type::Int, Type::Int],
								vec![Type::Int]
							)))
						),
						Tree::Identifier(
							"call".to_string(),
							Effect::function(
								vec![
									Type::Int,
									Type::Int,
									Type::Block(Effect::function(
										vec![Type::Int, Type::Int],
										vec![Type::Int]
									))
								],
								vec![Type::Int],
							),
						)
					],
					Effect::literal(Type::Block(Effect::function(vec![], vec![Type::Int])))
				),
				Tree::Identifier(
					"call".to_string(),
					Effect::function(
						vec![Type::Block(Effect::literal(Type::Int))],
						vec![Type::Int]
					)
				),
			],
			Effect::literal(Type::Block(Effect::literal(Type::Int)))
		)
	);
	let run_test = tokenizer(&"{1 2} call add call".chars().collect::<Vec<char>>());
	let run_test = Tree::Brackets(Bracket::Curly, rewriter(&parser(&run_test)), ());
	let mut run_test = compile(&typer(&run_test));
	run_test.extend([Push(-1), Shove(1), Goto, Label(-1)]);
	assert_eq!(run(&run_test), [3]);
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
	fn get_tag_mut(&mut self) -> &mut Tag {
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

#[derive(Clone, PartialEq)]
enum Type {
	Int,
	Bool,
	String,
	Block(Effect),
	Variable(usize),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Int => write!(f, "int"),
			Type::Bool => write!(f, "bool"),
			Type::String => write!(f, "string"),
			Type::Block(e) => write!(f, "{:?}", e),
			Type::Variable(_) => unreachable!("uneliminated var"),
		}
	}
}

impl Type {
	fn equalize(a: &Type, b: &Type, vars: &mut TypeVars) -> bool {
		match (a, b) {
			(Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::String, Type::String) => {
				true
			}
			(Type::Block(a), Type::Block(b)) => {
				a.inputs.iter().zip(&b.inputs).all(|(a, b)| Type::equalize(a, b, vars))
					&& a.outputs.iter().zip(&b.outputs).all(|(a, b)| Type::equalize(a, b, vars))
			}
			(Type::Variable(i), b) | (b, Type::Variable(i)) => vars.set_var(*i, b),
			_ => false,
		}
	}
}

#[derive(Clone, PartialEq)]
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

	fn compose(&mut self, other: &mut Effect, vars: &mut TypeVars) {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		assert!(self
			.outputs
			.drain(i..)
			.zip(&other.inputs[j..])
			.all(|(a, b)| Type::equalize(&a, b, vars)));
		self.inputs.extend(other.inputs[..j].iter().cloned());
		self.outputs.extend(other.outputs.iter().cloned());
	}
}

struct TypeVars(Vec<Option<Type>>);

impl TypeVars {
	fn new_var(self: &mut TypeVars) -> Type {
		self.0.push(None);
		Type::Variable(self.0.len() - 1)
	}

	fn old_var(self: &TypeVars) -> Type {
		Type::Variable(self.0.len() - 1)
	}

	fn set_var(self: &mut TypeVars, i: usize, a: &Type) -> bool {
		match &self.0[i] {
			None => {
				self.0[i] = Some(a.clone());
				true
			}
			Some(b) => Type::equalize(a, &b.clone(), self),
		}
	}

	fn get_var(self: &TypeVars, i: usize) -> &Type {
		match &self.0[i] {
			None => panic!("unsolved type var {} in {:?}", i, self.0),
			Some(Type::Variable(j)) => self.get_var(*j),
			Some(t) => t,
		}
	}
}

fn typer(tree: &Tree<()>) -> Tree<Effect> {
	fn typer(tree: &Tree<()>, vars: &mut TypeVars, scope: &mut Effect) -> Tree<Effect> {
		let mut out = match tree {
			Tree::Brackets(b, cs, ()) => {
				let mut child = Effect::function(vec![], vec![]);
				let cs: Vec<Tree<Effect>> =
					cs.iter().map(|c| typer(c, vars, &mut child)).collect();
				let t = match b {
					Bracket::Round => child,
					Bracket::Curly => Effect::literal(Type::Block(child)),
					Bracket::Square => todo!(),
				};
				Tree::Brackets(*b, cs, t)
			}
			Tree::String(s, ()) => Tree::String(s.clone(), Effect::literal(Type::String)),
			Tree::Identifier(i, ()) => Tree::Identifier(
				i.clone(),
				match i.as_str() {
					"call" => match scope.outputs.last() {
						Some(Type::Block(Effect { inputs, outputs })) => {
							let mut call_i = inputs.clone();
							call_i.push(Type::Block(Effect::function(
								inputs.clone(),
								outputs.clone(),
							)));
							Effect::function(call_i, outputs.clone())
						}
						t => panic!("call expected block, found {:?}", t),
					},
					"false" | "true" => Effect::literal(Type::Bool),
					"nop" => Effect::literal(Type::Block(Effect::function(vec![], vec![]))),
					"neg" => Effect::literal(Type::Block(Effect::function(
						vec![Type::Int],
						vec![Type::Int],
					))),
					"_not_" => Effect::literal(Type::Block(Effect::function(
						vec![Type::Bool],
						vec![Type::Bool],
					))),
					"mul" | "add" => Effect::literal(Type::Block(Effect::function(
						vec![Type::Int, Type::Int],
						vec![Type::Int],
					))),
					"eq" | "ne" => Effect::literal(Type::Block(Effect::function(
						vec![vars.new_var(), vars.old_var()],
						vec![Type::Bool],
					))),
					"lt" | "gt" | "le" | "ge" => Effect::literal(Type::Block(Effect::function(
						vec![Type::Int, Type::Int],
						vec![Type::Bool],
					))),
					"_and_" | "_or_" => Effect::literal(Type::Block(Effect::function(
						vec![Type::Bool, Type::Bool],
						vec![Type::Bool],
					))),
					"=" => todo!("value variables"),
					"_if_" | "_else_" => todo!("optionals"),
					"_while_" => Effect::literal(Type::Block(Effect::function(
						vec![
							Type::Block(Effect::literal(Type::Bool)),
							Type::Block(Effect::function(vec![], vec![])),
						],
						vec![],
					))),
					i => todo!("{}", i),
				},
			),
			Tree::Number(n, ()) => Tree::Number(*n, Effect::literal(Type::Int)),
			Tree::Operator(..) => unreachable!(),
		};
		scope.compose(out.get_tag_mut(), vars);
		out
	}
	fn replace_vars_in_effect(effect: &Effect, vars: &TypeVars) -> Effect {
		Effect {
			inputs: effect
				.inputs
				.iter()
				.map(|t| match t {
					Type::Variable(i) => vars.get_var(*i).clone(),
					t => t.clone(),
				})
				.collect(),
			outputs: effect
				.outputs
				.iter()
				.map(|t| match t {
					Type::Variable(i) => vars.get_var(*i).clone(),
					t => t.clone(),
				})
				.collect(),
		}
	}
	fn replace_vars_in_tree(tree: &Tree<Effect>, vars: &TypeVars) -> Tree<Effect> {
		match tree {
			Tree::Brackets(b, cs, e) => Tree::Brackets(
				*b,
				cs.iter().map(|c| replace_vars_in_tree(c, vars)).collect(),
				replace_vars_in_effect(e, vars),
			),
			Tree::String(s, e) => Tree::String(s.clone(), replace_vars_in_effect(e, vars)),
			Tree::Identifier(i, e) => {
				Tree::Identifier(i.clone(), replace_vars_in_effect(e, vars))
			}
			Tree::Number(n, e) => Tree::Number(*n, replace_vars_in_effect(e, vars)),
			Tree::Operator(..) => unreachable!(),
		}
	}
	let mut vars = TypeVars(vec![]);
	let mut scope = Effect::function(vec![], vec![]);
	let tree = typer(tree, &mut vars, &mut scope);
	if !scope.inputs.is_empty() {
		panic!("program expected {:?}", scope.inputs);
	}
	replace_vars_in_tree(&tree, &vars)
}

use Operation::*;
#[derive(Debug)]
enum Operation {
	Push(i64),
	IntNeg,
	IntMul,
	IntAdd,
	IntEq,
	IntLt,
	IntGt,
	BitNot,
	BitAnd,
	BitOr,
	Grab(usize),  // index from top of the stack
	Shove(usize), // index from top of the stack
	Label(i64),
	Goto,
}

fn compile(tree: &Tree<Effect>) -> Vec<Operation> {
	fn block(mut code: Vec<Operation>, rets: usize, labels: &mut i64) -> Vec<Operation> {
		let start = *labels;
		let end = *labels + 1;
		*labels += 2;
		code.extend([Grab(rets), Goto, Label(end)]);
		code.splice(0..0, [Push(start), Push(end), Goto, Label(start)]);
		code
	}
	fn compile(tree: &Tree<Effect>, labels: &mut i64) -> Vec<Operation> {
		match tree {
			Tree::Brackets(Bracket::Round, cs, _) => {
				cs.iter().flat_map(|c| compile(c, labels)).collect()
			}
			Tree::Brackets(Bracket::Curly, cs, Effect { outputs, .. }) => {
				let rets = match outputs.as_slice() {
					[Type::Block(Effect { outputs, .. })] => outputs.len(),
					_ => unreachable!(),
				};
				block(cs.iter().flat_map(|c| compile(c, labels)).collect(), rets, labels)
			}
			Tree::Brackets(Bracket::Square, ..) => todo!(),
			Tree::String(..) => todo!(),
			Tree::Identifier(i, Effect { inputs, outputs }) => {
				match (inputs.as_slice(), outputs.as_slice()) {
					([], [Type::Block(Effect { inputs, outputs })]) => {
						match (i.as_str(), inputs.as_slice(), outputs.as_slice()) {
							("nop", [], []) => block(vec![], 0, labels),
							("neg", [Type::Int], [Type::Int]) => block(vec![IntNeg], 1, labels),
							("not", [Type::Bool], [Type::Bool]) => {
								block(vec![BitNot], 1, labels)
							}
							("mul", [Type::Int, Type::Int], [Type::Int]) => {
								block(vec![IntMul], 1, labels)
							}
							("add", [Type::Int, Type::Int], [Type::Int]) => {
								block(vec![IntAdd], 1, labels)
							}
							("eq", [Type::Int, Type::Int], [Type::Bool])
							| ("eq", [Type::Bool, Type::Bool], [Type::Bool]) => block(vec![IntEq], 1, labels),
							("ne", [Type::Int, Type::Int], [Type::Bool])
							| ("ne", [Type::Bool, Type::Bool], [Type::Bool]) => block(vec![IntEq, BitNot], 1, labels),
							("lt", [Type::Int, Type::Int], [Type::Bool]) => {
								block(vec![IntLt], 1, labels)
							}
							("gt", [Type::Int, Type::Int], [Type::Bool]) => {
								block(vec![IntGt], 1, labels)
							}
							("le", [Type::Int, Type::Int], [Type::Bool]) => {
								block(vec![IntGt, BitNot], 1, labels)
							}
							("ge", [Type::Int, Type::Int], [Type::Bool]) => {
								block(vec![IntLt, BitNot], 1, labels)
							}
							("_and_", [Type::Bool, Type::Bool], [Type::Bool]) => {
								block(vec![BitAnd], 1, labels)
							}
							("_or_", [Type::Bool, Type::Bool], [Type::Bool]) => {
								block(vec![BitOr], 1, labels)
							}
							(s, i, o) => {
								todo!("could not compile {:?} with {:?} and {:?}", s, i, o)
							}
						}
					}
					(inputs, outputs) => match (i.as_str(), inputs, outputs) {
						("call", _, _) => {
							let ret = *labels;
							*labels += 1;
							vec![Push(ret), Shove(inputs.len()), Goto, Label(ret)]
						}
						("false", [], [Type::Bool]) => vec![Push(0)],
						("true", [], [Type::Bool]) => vec![Push(1)],
						(s, i, o) => todo!("could not compile {:?} with {:?} and {:?}", s, i, o),
					},
				}
			}
			Tree::Number(n, _) => vec![Push(*n)],
			Tree::Operator(..) => unreachable!(),
		}
	}
	compile(tree, &mut 0)
}

fn run(code: &[Operation]) -> Vec<i64> {
	let mut stack = vec![];
	let mut i = 0;
	while i < code.len() {
		match code[i] {
			Push(i) => stack.push(i),
			IntMul => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push(a * b);
			}
			IntAdd => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push(a + b);
			}
			IntNeg => {
				let a = stack.pop().unwrap();
				stack.push(-a);
			}
			IntEq => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push((a == b) as i64);
			}
			IntLt => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push((a < b) as i64);
			}
			IntGt => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push((a > b) as i64);
			}
			BitNot => {
				let a = stack.pop().unwrap();
				stack.push(!a);
			}
			BitAnd => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push((a & b) as i64);
			}
			BitOr => {
				let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
				stack.push((a | b) as i64);
			}
			Grab(i) => {
				let a = stack.remove(stack.len() - 1 - i);
				stack.push(a);
			}
			Shove(i) => {
				let a = stack.pop().unwrap();
				stack.insert(stack.len() - i, a);
			}
			Label(_) => {}
			Goto => {
				let a = stack.pop().unwrap();
				i = code
					.iter()
					.position(|o| matches!(o, Label(b) if a == *b))
					.expect("could not find label")
			}
		}
		i += 1;
	}
	stack
}
