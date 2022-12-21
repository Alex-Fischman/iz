fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let tree = Tree::Brackets(Bracket::Curly, trees);
	let tree = scoper(&tree);
	let tree = typer(&tree);
	let mut code = compile(&tree);
	code.extend([Push(-1), Shove(1), Goto, Label(-1)]);
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
			Number(3),
			Number(2),
			Identifier("true".to_string()),
			Identifier("_if_".to_string()),
			Identifier("call".to_string()),
			Identifier("_else_".to_string()),
			Identifier("call".to_string()),
			Number(1),
			Identifier("false".to_string()),
			Identifier("_if_".to_string()),
			Identifier("call".to_string()),
			Identifier("_else_".to_string()),
			Identifier("call".to_string()),
			Number(3),
			Brackets(Bracket::Round, vec![Number(2)]),
			Identifier("mul".to_string()),
			Identifier("call".to_string()),
			Number(1),
			Identifier("add".to_string()),
			Identifier("call".to_string()),
			Identifier("a".to_string()),
			Identifier("=".to_string()),
			Identifier("call".to_string()),
		]
	);
	let typer_test = tokenizer(&"{1 2} call add call".chars().collect::<Vec<char>>());
	let typer_test = typer(&scoper(&Tree::Brackets(Bracket::Curly, parser(&typer_test))));
	assert_eq!(
		typer_test,
		Typed::Block(
			vec![
				Typed::Block(
					vec![Typed::Number(1), Typed::Number(2),],
					Effect::function(vec![], vec![Int, Int])
				),
				Typed::Identifier(
					"call".to_string(),
					Effect::new(
						vec![Block(Effect::new(vec![], vec![Int, Int]))],
						vec![Int, Int]
					)
				),
				Typed::Identifier(
					"add".to_string(),
					Effect::function(vec![Int, Int], vec![Int])
				),
				Typed::Identifier(
					"call".to_string(),
					Effect::new(
						vec![Int, Int, Block(Effect::new(vec![Int, Int], vec![Int]))],
						vec![Int],
					),
				)
			],
			Effect::function(vec![], vec![Int])
		)
	);
	let run_test = tokenizer(&"add@(1 2) 1 2 add@() 1 + 2".chars().collect::<Vec<char>>());
	let mut run_test =
		compile(&typer(&scoper(&Tree::Brackets(Bracket::Curly, parser(&run_test)))));
	run_test.extend([Push(-1), Shove(1), Goto, Label(-1)]);
	assert_eq!(run(&run_test), [3, 3, 3]);
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
						panic!("no digits after base specifier")
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
								panic!("invalid digit {} in base {}", digit, base)
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
	(&[("var", "var", 0, 1)], true),
	(&[("=", "=", 1, 1)], true),
	(&[("if", "_if_", 0, 2), ("while", "_while_", 0, 2)], true),
	(&[("else", "_else_", 1, 1)], true),
];

#[derive(Clone, Debug, PartialEq)]
enum Tree {
	Brackets(Bracket, Vec<Tree>),
	String(String),
	Identifier(String),
	Number(i64),
	Operator(String, Vec<Tree>),
}

fn parser(tokens: &[Token]) -> Vec<Tree> {
	fn parse(tokens: &[Token], i: &mut usize, mut searching: Option<Bracket>) -> Vec<Tree> {
		let mut trees = Vec::new();
		while *i < tokens.len() {
			let token = &tokens[*i];
			*i += 1;
			trees.push(match token {
				Token::Bracket(b, Side::Open) => Tree::Brackets(*b, parse(tokens, i, Some(*b))),
				Token::Bracket(b, Side::Close) => match (b, searching) {
					(b, Some(c)) if *b == c => {
						searching = None;
						break;
					}
					(b, _) => panic!("extra {}", b.to_char(Side::Close)),
				},
				Token::String(s) => Tree::String(s.clone()),
				Token::Identifier(i) => Tree::Identifier(i.clone()),
				Token::Number(n) => Tree::Number(*n),
			});
		}
		if let Some(b) = searching {
			panic!("extra {}", b.to_char(Side::Open))
		}
		for (operators, right) in OPERATORS {
			let mut j = if *right { trees.len().wrapping_sub(1) } else { 0 };
			while let Some(tree) = trees.get(j) {
				if let Tree::Identifier(i) = tree {
					let i = i.clone();
					if let Some(operator) = operators.iter().find(|op| op.0 == i) {
						if j < operator.2 || j + operator.3 >= trees.len() {
							panic!("not enough operator arguments for {}", i)
						}
						trees.remove(j);
						let cs: Vec<Tree> =
							trees.drain(j - operator.2..j + operator.3).collect();
						j -= operator.2;
						trees.insert(j, Tree::Operator(i, cs));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		trees
	}
	fn rewriter(trees: &[Tree]) -> Vec<Tree> {
		let mut trees = Vec::from(trees);
		let mut j = 0;
		while j < trees.len() {
			j += match trees[j].clone() {
				Tree::Brackets(b, cs) => {
					trees[j] = Tree::Brackets(b, rewriter(&cs));
					1
				}
				Tree::Operator(i, cs) => {
					let operator = OPERATORS
						.iter()
						.find_map(|(ops, _)| ops.iter().find(|op| op.0 == i))
						.unwrap();
					let mut cs = cs.clone();
					cs.reverse();
					let cs = rewriter(&cs);
					trees.insert(j + 1, Tree::Identifier(operator.1.to_owned()));
					trees.insert(j + 2, Tree::Identifier("call".to_string()));
					let out = cs.len() + 1;
					trees.splice(j..j + 1, cs);
					out
				}
				_ => 1,
			}
		}
		trees
	}
	rewriter(&parse(tokens, &mut 0, None))
}

#[derive(Clone, Debug, PartialEq)]
enum Scoped {
	Group(Vec<Scoped>),
	Block(Vec<Scoped>, Scope),
	String(String),
	Identifier(String),
	Number(i64),
}

use std::cell::RefCell;
use std::rc::Rc;
type Scope = Rc<RefCell<S>>;
#[derive(Clone, Debug, PartialEq)]
struct S {
	vars: Vec<String>,
	parent: Option<Scope>,
}

impl S {
	fn new(parent: Option<Scope>) -> S {
		S { vars: vec![], parent }
	}
}

fn scoper(tree: &Tree) -> Scoped {
	fn scoper(tree: &Tree, scope: Option<Scope>) -> Scoped {
		match tree {
			Tree::Brackets(Bracket::Round, cs) => {
				Scoped::Group(cs.iter().map(|c| scoper(c, scope.clone())).collect())
			}
			Tree::Brackets(Bracket::Curly, cs) => {
				let s = Rc::new(RefCell::new(S::new(scope)));
				Scoped::Block(cs.iter().map(|c| scoper(c, Some(s.clone()))).collect(), s)
			}
			Tree::Brackets(Bracket::Square, _cs) => todo!(),
			Tree::String(s) => Scoped::String(s.clone()),
			Tree::Identifier(_i) => todo!(),
			Tree::Number(n) => Scoped::Number(*n),
			Tree::Operator(..) => unreachable!(),
		}
	}
	scoper(tree, None)
}

#[derive(Clone, Debug, PartialEq)]
enum Typed {
	Group(Vec<Typed>, Effect),
	Block(Vec<Typed>, Effect),
	String(String),
	Identifier(String, Effect),
	Number(i64),
}

use Type::*;
#[derive(Clone, PartialEq)]
enum Type {
	Int,
	Bool,
	Str,
	Block(Effect),
	Variable(usize),
}

impl std::fmt::Debug for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Int => write!(f, "int"),
			Bool => write!(f, "bool"),
			Str => write!(f, "string"),
			Block(e) => write!(f, "{:?}", e),
			Variable(_) => panic!("uneliminated var"),
		}
	}
}

impl Type {
	fn equalize(a: &Type, b: &Type, vars: &mut TypeVars) -> bool {
		match (a, b) {
			(Int, Int) | (Bool, Bool) | (Str, Str) => true,
			(Block(a), Block(b)) => {
				a.inputs.iter().zip(&b.inputs).all(|(a, b)| Type::equalize(a, b, vars))
					&& a.outputs.iter().zip(&b.outputs).all(|(a, b)| Type::equalize(a, b, vars))
			}
			(Variable(i), b) | (b, Variable(i)) => vars.set_var(*i, b),
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
	fn new(inputs: Vec<Type>, outputs: Vec<Type>) -> Effect {
		Effect { inputs, outputs }
	}

	fn literal(t: Type) -> Effect {
		Effect { inputs: vec![], outputs: vec![t] }
	}

	fn function(inputs: Vec<Type>, outputs: Vec<Type>) -> Effect {
		Effect::literal(Block(Effect { inputs, outputs }))
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
		Variable(self.0.len() - 1)
	}

	fn old_var(self: &TypeVars) -> Type {
		Variable(self.0.len() - 1)
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
			None => panic!("unsolved type var: likely due to unbound identifier"),
			Some(Variable(j)) => self.get_var(*j),
			Some(t) => t,
		}
	}
}

fn typer(tree: &Scoped) -> Typed {
	fn typer(tree: &Scoped, vars: &mut TypeVars, effect: &mut Effect) -> Typed {
		let mut out = match tree {
			Scoped::Group(cs) => {
				let mut e = Effect::new(vec![], vec![]);
				let cs: Vec<Typed> = cs.iter().map(|c| typer(c, vars, &mut e)).collect();
				Typed::Group(cs, e)
			}
			Scoped::Block(cs, _vs) => {
				let mut e = Effect::new(vec![], vec![]);
				let cs: Vec<Typed> = cs.iter().map(|c| typer(c, vars, &mut e)).collect();
				Typed::Block(cs, Effect::function(e.inputs, e.outputs));
				todo!("local variables in typer");
			}
			Scoped::String(s) => Typed::String(s.clone()),
			Scoped::Identifier(i) => Typed::Identifier(
				i.clone(),
				match i.as_str() {
					"call" => match effect.outputs.last() {
						Some(Block(Effect { inputs, outputs })) => {
							let mut i = inputs.clone();
							i.push(Block(Effect::new(inputs.clone(), outputs.clone())));
							Effect::new(i, outputs.clone())
						}
						t => panic!("call expected block, found {:?}", t),
					},
					"false" | "true" => Effect::literal(Bool),
					"nop" => Effect::function(vec![], vec![]),
					"neg" => Effect::function(vec![Int], vec![Int]),
					"_not_" => Effect::function(vec![Bool], vec![Bool]),
					"mul" | "add" => Effect::function(vec![Int, Int], vec![Int]),
					"eq" | "ne" => {
						Effect::function(vec![vars.new_var(), vars.old_var()], vec![Bool])
					}
					"lt" | "gt" | "le" | "ge" => Effect::function(vec![Int, Int], vec![Bool]),
					"_and_" | "_or_" => Effect::function(vec![Bool, Bool], vec![Bool]),
					"var" => todo!("var"),
					"=" => todo!("="),
					"_if_" | "_else_" => todo!("optionals"),
					"_while_" => Effect::function(
						vec![Block(Effect::literal(Bool)), Block(Effect::new(vec![], vec![]))],
						vec![],
					),
					_ => Effect::literal(vars.new_var()),
				},
			),
			Scoped::Number(n) => Typed::Number(*n),
		};
		match &mut out {
			Typed::Group(_, e) | Typed::Block(_, e) | Typed::Identifier(_, e) => {
				effect.compose(e, vars)
			}
			Typed::String(_) => effect.compose(&mut Effect::literal(Str), vars),
			Typed::Number(_) => effect.compose(&mut Effect::literal(Int), vars),
		};
		out
	}
	fn replace_vars(tree: &mut Typed, vars: &TypeVars) {
		if let Typed::Group(_, e) | Typed::Block(_, e) | Typed::Identifier(_, e) = tree {
			*e = Effect {
				inputs: e
					.inputs
					.iter()
					.map(|t| match t {
						Variable(i) => vars.get_var(*i).clone(),
						t => t.clone(),
					})
					.collect(),
				outputs: e
					.outputs
					.iter()
					.map(|t| match t {
						Variable(i) => vars.get_var(*i).clone(),
						t => t.clone(),
					})
					.collect(),
			};
		}
		if let Typed::Group(cs, _) | Typed::Block(cs, _) = tree {
			cs.iter_mut().for_each(|c| replace_vars(c, vars));
		}
	}
	let mut vars = TypeVars(vec![]);
	let mut effect = Effect::new(vec![], vec![]);
	let mut tree = typer(tree, &mut vars, &mut effect);
	if !effect.inputs.is_empty() {
		panic!("program expected {:?}", effect.inputs);
	}
	replace_vars(&mut tree, &vars);
	tree
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

fn compile(tree: &Typed) -> Vec<Operation> {
	fn block(mut code: Vec<Operation>, rets: usize, labels: &mut i64) -> Vec<Operation> {
		let start = *labels;
		let end = *labels + 1;
		*labels += 2;
		code.splice(0..0, [Push(start), Push(end), Goto, Label(start)]);
		code.extend([Grab(rets), Goto, Label(end)]);
		code
	}
	fn compile(tree: &Typed, labels: &mut i64) -> Vec<Operation> {
		match tree {
			Typed::Group(cs, _) => cs.iter().flat_map(|c| compile(c, labels)).collect(),
			Typed::Block(cs, Effect { outputs, .. }) => {
				let rets = match outputs.as_slice() {
					[Block(Effect { outputs, .. })] => outputs.len(),
					_ => unreachable!(),
				};
				// pop local vars here?
				block(cs.iter().flat_map(|c| compile(c, labels)).collect(), rets, labels)
			}
			Typed::String(_) => todo!(),
			Typed::Identifier(i, Effect { inputs, outputs }) => {
				match (inputs.as_slice(), outputs.as_slice()) {
					([], [Block(Effect { inputs, outputs })]) => block(
						match (i.as_str(), inputs.as_slice(), outputs.as_slice()) {
							("neg", [Int], [Int]) => vec![IntNeg],
							("_not_", [Bool], [Bool]) => vec![BitNot],
							("mul", [Int, Int], [Int]) => vec![IntMul],
							("add", [Int, Int], [Int]) => vec![IntAdd],
							("eq", [Int, Int], [Bool]) | ("eq", [Bool, Bool], [Bool]) => {
								vec![IntEq]
							}
							("ne", [Int, Int], [Bool]) | ("ne", [Bool, Bool], [Bool]) => {
								vec![IntEq, BitNot]
							}
							("lt", [Int, Int], [Bool]) => vec![IntLt],
							("gt", [Int, Int], [Bool]) => vec![IntGt],
							("le", [Int, Int], [Bool]) => vec![IntGt, BitNot],
							("ge", [Int, Int], [Bool]) => vec![IntLt, BitNot],
							("_and_", [Bool, Bool], [Bool]) => vec![BitAnd],
							("_or_", [Bool, Bool], [Bool]) => vec![BitOr],
							(s, i, o) => {
								todo!("could not compile {:?} with {:?} and {:?}", s, i, o)
							}
						},
						1,
						labels,
					),
					(inputs, outputs) => match (i.as_str(), inputs, outputs) {
						("call", _, _) => {
							let ret = *labels;
							*labels += 1;
							vec![Push(ret), Shove(inputs.len()), Goto, Label(ret)]
						}
						("false", [], [Bool]) => vec![Push(0)],
						("true", [], [Bool]) => vec![Push(1)],
						("nop", [], []) => block(vec![], 0, labels),
						(s, i, o) => todo!("could not compile {:?} with {:?} and {:?}", s, i, o),
					},
				}
			}
			Typed::Number(n) => vec![Push(*n)],
		}
	}
	compile(tree, &mut 0)
}

fn run(code: &[Operation]) -> Vec<i64> {
	let mut stack = vec![];
	let binary = |stack: &mut Vec<i64>, f: &dyn Fn(i64, i64) -> i64| {
		let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
		stack.push(f(a, b));
	};
	let unary = |stack: &mut Vec<i64>, f: &dyn Fn(i64) -> i64| {
		let a = stack.pop().unwrap();
		stack.push(f(a));
	};
	let mut i = 0;
	while i < code.len() {
		match code[i] {
			Push(i) => stack.push(i),
			IntMul => binary(&mut stack, &|a, b| a * b),
			IntAdd => binary(&mut stack, &|a, b| a + b),
			IntNeg => unary(&mut stack, &|a| -a),
			IntEq => binary(&mut stack, &|a, b| (a == b) as i64),
			IntLt => binary(&mut stack, &|a, b| (a < b) as i64),
			IntGt => binary(&mut stack, &|a, b| (a > b) as i64),
			BitNot => unary(&mut stack, &|a| !a),
			BitAnd => binary(&mut stack, &|a, b| a & b),
			BitOr => binary(&mut stack, &|a, b| a | b),
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
