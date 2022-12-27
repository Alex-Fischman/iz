fn main() {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).expect("no file passed");
	let chars: Vec<char> =
		std::fs::read_to_string(file).expect("could not read file").chars().collect();

	let tokens = tokenizer(&chars);
	let trees = parser(&tokens);
	let tree = rewriter(&trees);
	let tree = typer(&tree);
	let code = compile(&tree);
	let stack = run(&code);
	println!("{:?}", stack);
}

#[test]
fn test() {
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
	let parse_test = "if false 1 else if true 2 else 3\n# asdf\nvar a a = 1 + (2) * 3";
	assert_eq!(
		rewriter(&parser(&tokenizer(&parse_test.chars().collect::<Vec<char>>()))),
		Rewritten::Block(vec![
			Rewritten::Number(3),
			Rewritten::Number(2),
			Rewritten::Identifier("true".to_string()),
			Rewritten::Identifier("_if_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Identifier("_else_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Number(1),
			Rewritten::Identifier("false".to_string()),
			Rewritten::Identifier("_if_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Identifier("_else_".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Declaration("a".to_string()),
			Rewritten::Number(3),
			Rewritten::Group(vec![Rewritten::Number(2)]),
			Rewritten::Identifier("mul".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Number(1),
			Rewritten::Identifier("add".to_string()),
			Rewritten::Identifier("call".to_string()),
			Rewritten::Assignment("a".to_string()),
		])
	);
	let typer_test = "{1 2} call add call var a a = 2 a".chars().collect::<Vec<char>>();
	let ts = new_scope(None);
	set_var(ts.clone(), "a".to_string(), Int);
	assert_eq!(
		typer(&rewriter(&parser(&tokenizer(&typer_test)))),
		Typed::Block(
			vec![
				Typed::Block(
					vec![Typed::Number(1), Typed::Number(2)],
					Effect::function(vec![], vec![Int, Int]),
					new_scope(Some(ts.clone()))
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
				),
				Typed::Declaration("a".to_string()),
				Typed::Number(2),
				Typed::Assignment("a".to_string(), Int),
				Typed::Identifier("a".to_string(), Effect::literal(Int)),
			],
			Effect::function(vec![], vec![Int, Int]),
			ts,
		)
	);
	let run_test = tokenizer(&"add@(1 2) 1 2 add@() 1 + 2".chars().collect::<Vec<char>>());
	let mut stack = Stack::new();
	[3i64, 3, 3].into_iter().for_each(|v| stack.push(v));
	assert_eq!(run(&compile(&typer(&rewriter(&parser(&run_test))))), stack);
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
enum Parsed {
	Brackets(Bracket, Vec<Parsed>),
	String(String),
	Identifier(String),
	Number(i64),
	Operator(String, Vec<Parsed>),
}

fn parser(tokens: &[Token]) -> Vec<Parsed> {
	fn parse(tokens: &[Token], i: &mut usize, mut searching: Option<Bracket>) -> Vec<Parsed> {
		let mut trees = Vec::new();
		while *i < tokens.len() {
			let token = &tokens[*i];
			*i += 1;
			trees.push(match token {
				Token::Bracket(b, Side::Open) => {
					Parsed::Brackets(*b, parse(tokens, i, Some(*b)))
				}
				Token::Bracket(b, Side::Close) => match (b, searching) {
					(b, Some(c)) if *b == c => {
						searching = None;
						break;
					}
					(b, _) => panic!("extra {}", b.to_char(Side::Close)),
				},
				Token::String(s) => Parsed::String(s.clone()),
				Token::Identifier(i) => Parsed::Identifier(i.clone()),
				Token::Number(n) => Parsed::Number(*n),
			});
		}
		if let Some(b) = searching {
			panic!("extra {}", b.to_char(Side::Open))
		}
		for (operators, right) in OPERATORS {
			let mut j = if *right { trees.len().wrapping_sub(1) } else { 0 };
			while let Some(tree) = trees.get(j) {
				if let Parsed::Identifier(i) = tree {
					let i = i.clone();
					if let Some(operator) = operators.iter().find(|op| op.0 == i) {
						if j < operator.2 || j + operator.3 >= trees.len() {
							panic!("not enough operator arguments for {}", i)
						}
						trees.remove(j);
						let cs: Vec<Parsed> =
							trees.drain(j - operator.2..j + operator.3).collect();
						j -= operator.2;
						trees.insert(j, Parsed::Operator(i, cs));
					}
				}
				j = if *right { j.wrapping_sub(1) } else { j + 1 }
			}
		}
		trees
	}
	parse(tokens, &mut 0, None)
}

#[derive(Clone, Debug, PartialEq)]
enum Rewritten {
	Group(Vec<Rewritten>),
	Block(Vec<Rewritten>),
	String(String),
	Identifier(String),
	Number(i64),
	Declaration(String),
	Assignment(String),
}

fn rewriter(trees: &[Parsed]) -> Rewritten {
	fn rewriter(trees: &[Parsed]) -> Vec<Rewritten> {
		let mut out = vec![];
		for tree in trees {
			match tree {
				Parsed::Brackets(Bracket::Round, cs) => out.push(Rewritten::Group(rewriter(cs))),
				Parsed::Brackets(Bracket::Curly, cs) => {
					out.push(Rewritten::Block(rewriter(cs)));
				}
				Parsed::Brackets(Bracket::Square, _cs) => todo!(),
				Parsed::String(s) => out.push(Rewritten::String(s.clone())),
				Parsed::Identifier(i) => out.push(Rewritten::Identifier(i.clone())),
				Parsed::Number(n) => out.push(Rewritten::Number(*n)),
				Parsed::Operator(i, cs) => match i.as_str() {
					"var" => match cs.as_slice() {
						[Parsed::Identifier(v)] => out.push(Rewritten::Declaration(v.clone())),
						cs => panic!("expected one var name, found {:?}", cs),
					},
					"=" => match cs.as_slice() {
						[Parsed::Identifier(v), rhs] => {
							out.append(&mut rewriter(&[rhs.clone()]));
							out.push(Rewritten::Assignment(v.clone()));
						}
						cs => panic!("expected one var name and one value, found {:?}", cs),
					},
					_ => {
						let operator = OPERATORS
							.iter()
							.find_map(|(ops, _)| ops.iter().find(|op| op.0 == i))
							.unwrap();
						let mut cs = cs.clone();
						cs.reverse();
						out.append(&mut rewriter(&cs));
						out.push(Rewritten::Identifier(operator.1.to_string()));
						out.push(Rewritten::Identifier("call".to_string()));
					}
				},
			}
		}
		out
	}
	Rewritten::Block(rewriter(trees))
}

#[derive(Clone, Debug, PartialEq)]
enum Typed {
	Group(Vec<Typed>, Effect),
	Block(Vec<Typed>, Effect, Scope<Type>),
	String(String),
	Identifier(String, Effect),
	Number(i64),
	Declaration(String),
	Assignment(String, Type),
}

impl Typed {
	fn get_effect(&self) -> Effect {
		match self {
			Typed::Group(_, e) | Typed::Block(_, e, _) | Typed::Identifier(_, e) => e.clone(),
			Typed::Declaration(_) => Effect::new(vec![], vec![]),
			Typed::Assignment(_, t) => Effect::new(vec![t.clone()], vec![]),
			Typed::String(_) => Effect::literal(Str),
			Typed::Number(_) => Effect::literal(Int),
		}
	}
}

use Type::*;
#[derive(Clone, Debug, PartialEq)]
enum Type {
	Int,
	Bool,
	Str,
	Block(Effect),
	Variable(usize),
}

impl Type {
	fn size_of(&self) -> usize {
		match self {
			Int => 8,
			Bool => 1,
			Str => todo!(),
			Block(_) => 8, // label
			Variable(_) => unreachable!(),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
struct Effect {
	inputs: Vec<Type>,
	outputs: Vec<Type>,
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

	fn compose(&mut self, other: &Effect, vars: &mut TypeVars) {
		let (x, y) = (other.inputs.len(), self.outputs.len());
		let (i, j) = if x <= y { (y - x, 0) } else { (0, x - y) };
		assert!(self
			.outputs
			.drain(i..)
			.zip(&other.inputs[j..])
			.all(|(a, b)| vars.equalize(&a, b)));
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
			Some(b) => self.equalize(a, &b.clone()),
			None => {
				self.0[i] = Some(a.clone());
				true
			}
		}
	}

	fn get_var(self: &TypeVars, i: usize) -> &Type {
		match &self.0[i] {
			None => panic!("unsolved type var in {:?}", self.0),
			Some(Variable(j)) => self.get_var(*j),
			Some(t) => t,
		}
	}

	fn equalize(&mut self, a: &Type, b: &Type) -> bool {
		match (a, b) {
			(Int, Int) | (Bool, Bool) | (Str, Str) => true,
			(Block(a), Block(b)) => {
				a.inputs.iter().zip(&b.inputs).all(|(a, b)| self.equalize(a, b))
					&& a.outputs.iter().zip(&b.outputs).all(|(a, b)| self.equalize(a, b))
			}
			(Variable(i), b) | (b, Variable(i)) => self.set_var(*i, b),
			_ => false,
		}
	}

	fn substitute_type(&self, t: &Type) -> Type {
		match t {
			Int => Int,
			Bool => Bool,
			Str => Str,
			Block(e) => Block(self.substitute_effect(e)),
			Variable(i) => self.get_var(*i).clone(),
		}
	}

	fn substitute_effect(&self, e: &Effect) -> Effect {
		Effect {
			inputs: e.inputs.iter().map(|t| self.substitute_type(t)).collect(),
			outputs: e.outputs.iter().map(|t| self.substitute_type(t)).collect(),
		}
	}

	fn substitute(&self, typed: &mut Typed) {
		match typed {
			Typed::Group(_, e) | Typed::Block(_, e, _) | Typed::Identifier(_, e) => {
				*e = self.substitute_effect(e)
			}
			Typed::Assignment(_, t) => *t = self.substitute_type(t),
			Typed::Declaration(_) | Typed::Number(_) | Typed::String(_) => {}
		}
		if let Typed::Block(_, _, scope) = typed {
			scope.borrow_mut().vars.values_mut().for_each(|t| *t = self.substitute_type(t))
		}
		if let Typed::Group(cs, _) | Typed::Block(cs, _, _) = typed {
			cs.iter_mut().for_each(|c| self.substitute(c))
		}
	}
}

use std::{cell::RefCell, collections::BTreeMap, rc::Rc}; // avoiding HashMap to avoid random order
type Scope<T> = Rc<RefCell<S<T>>>;
#[derive(Clone, Debug, PartialEq)]
struct S<T> {
	vars: BTreeMap<String, T>,
	parent: Option<Scope<T>>,
}

fn new_scope<T>(parent: Option<Scope<T>>) -> Scope<T> {
	Rc::new(RefCell::new(S { vars: BTreeMap::new(), parent }))
}

fn set_var<T>(scope: Scope<T>, name: String, value: T) {
	find_scope(scope.clone(), &name).unwrap_or(scope).borrow_mut().vars.insert(name, value);
}

fn get_var<T: Clone>(scope: Scope<T>, name: &str) -> Option<T> {
	find_scope(scope.clone(), name).unwrap_or(scope).borrow().vars.get(name).cloned()
}

fn find_scope<T>(scope: Scope<T>, name: &str) -> Option<Scope<T>> {
	if scope.borrow().vars.contains_key(name) {
		Some(scope)
	} else if let Some(parent) = &scope.borrow().parent {
		find_scope(parent.clone(), name)
	} else {
		None
	}
}

fn typer(tree: &Rewritten) -> Typed {
	fn typer(
		tree: &Rewritten,
		vars: &mut TypeVars,
		effect: &mut Effect,
		scope: Option<Scope<Type>>,
	) -> Typed {
		let out = match tree {
			Rewritten::Group(cs) => {
				let mut e = Effect::new(vec![], vec![]);
				let cs: Vec<Typed> =
					cs.iter().map(|c| typer(c, vars, &mut e, scope.clone())).collect();
				Typed::Group(cs, e)
			}
			Rewritten::Block(cs) => {
				let mut e = Effect::new(vec![], vec![]);
				let s = new_scope(scope);
				let cs: Vec<Typed> =
					cs.iter().map(|c| typer(c, vars, &mut e, Some(s.clone()))).collect();
				Typed::Block(cs, Effect::function(e.inputs, e.outputs), s)
			}
			Rewritten::String(s) => Typed::String(s.clone()),
			Rewritten::Identifier(i) => Typed::Identifier(
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
					"nop" => Effect::new(vec![], vec![]),
					"false" | "true" => Effect::literal(Bool),
					"neg" => Effect::function(vec![Int], vec![Int]),
					"_not_" => Effect::function(vec![Bool], vec![Bool]),
					"mul" | "add" => Effect::function(vec![Int, Int], vec![Int]),
					"eq" | "ne" => {
						Effect::function(vec![vars.new_var(), vars.old_var()], vec![Bool])
					}
					"lt" | "gt" | "le" | "ge" => Effect::function(vec![Int, Int], vec![Bool]),
					"_and_" | "_or_" => Effect::function(vec![Bool, Bool], vec![Bool]),
					"_if_" | "_else_" => todo!("optionals"),
					"_while_" => Effect::function(
						vec![Block(Effect::literal(Bool)), Block(Effect::new(vec![], vec![]))],
						vec![],
					),
					i => match get_var(scope.unwrap(), i) {
						Some(v) => Effect::literal(v),
						None => todo!("could not type {:?}", i),
					},
				},
			),
			Rewritten::Number(n) => Typed::Number(*n),
			Rewritten::Declaration(i) => {
				if get_var(scope.clone().unwrap(), i).is_some() {
					todo!("redefined local variables")
				}
				set_var(scope.unwrap(), i.clone(), vars.new_var());
				Typed::Declaration(i.clone())
			}
			Rewritten::Assignment(i) => match get_var(scope.unwrap(), i) {
				Some(v) => Typed::Assignment(i.clone(), v),
				None => todo!("could not type {:?}", i),
			},
		};
		effect.compose(&out.get_effect(), vars);
		out
	}
	let mut vars = TypeVars(vec![]);
	let mut effect = Effect::new(vec![], vec![]);
	let mut tree = typer(tree, &mut vars, &mut effect, None);
	if !effect.inputs.is_empty() {
		panic!("program expected {:?}", effect.inputs);
	}
	vars.substitute(&mut tree);
	tree
}

use Operation::*;
#[derive(Clone, Debug, PartialEq)]
enum Operation {
	IntPush(i64),
	IntNeg,
	IntMul,
	IntAdd,
	IntEq,
	IntLt,
	IntGt,
	BoolPush(bool),
	BoolNot,
	BoolAnd,
	BoolOr,
	BoolEq,
	Alloc(usize /* size */),
	Free(usize /* size */),
	Move(usize /* start */, usize /* end */, usize /* size */), // indices from top of stack
	Label(i64),
	Goto,
}

fn compile(tree: &Typed) -> Vec<Operation> {
	fn block(mut code: Vec<Operation>, rets_size: usize, labels: &mut i64) -> Vec<Operation> {
		let (start, end) = (*labels, *labels + 1);
		*labels += 2;
		code.splice(0..0, [IntPush(start), IntPush(end), Goto, Label(start)]);
		code.extend([Move(rets_size, 0, 8), Goto, Label(end)]);
		code
	}
	fn compile(tree: &Typed, labels: &mut i64) -> Vec<Operation> {
		match tree {
			Typed::Group(cs, _) => cs.iter().flat_map(|c| compile(c, labels)).collect(),
			Typed::Block(cs, Effect { outputs, .. }, scope) => {
				let e = match outputs.as_slice() {
					[Block(e)] => e,
					_ => unreachable!(),
				};
				let args_size = e.inputs.iter().map(Type::size_of).sum();
				let rets_size = e.outputs.iter().map(Type::size_of).sum();
				let vars_size: usize = scope.borrow().vars.values().map(Type::size_of).sum();

				let mut code: Vec<Operation> =
					cs.iter().flat_map(|c| compile(c, labels)).collect();
				code.splice(0..0, [Alloc(vars_size), Move(0, args_size, vars_size)]);
				code.extend([Move(rets_size, 0, vars_size), Free(vars_size)]);
				block(code, rets_size, labels)
			}
			Typed::String(_) => todo!(),
			Typed::Identifier(i, Effect { inputs, outputs }) => {
				match (inputs.as_slice(), outputs.as_slice()) {
					([], [Block(Effect { inputs, outputs })]) => block(
						match (i.as_str(), inputs.as_slice(), outputs.as_slice()) {
							("neg", [Int], [Int]) => vec![IntNeg],
							("_not_", [Bool], [Bool]) => vec![BoolNot],
							("mul", [Int, Int], [Int]) => vec![IntMul],
							("add", [Int, Int], [Int]) => vec![IntAdd],
							("eq", [Int, Int], [Bool]) => vec![IntEq],
							("eq", [Bool, Bool], [Bool]) => vec![BoolEq],
							("ne", [Int, Int], [Bool]) => vec![IntEq, BoolNot],
							("ne", [Bool, Bool], [Bool]) => vec![BoolEq, BoolNot],
							("lt", [Int, Int], [Bool]) => vec![IntLt],
							("gt", [Int, Int], [Bool]) => vec![IntGt],
							("le", [Int, Int], [Bool]) => vec![IntGt, BoolNot],
							("ge", [Int, Int], [Bool]) => vec![IntLt, BoolNot],
							("_and_", [Bool, Bool], [Bool]) => vec![BoolAnd],
							("_or_", [Bool, Bool], [Bool]) => vec![BoolOr],
							(s, i, o) => todo!("could not compile {:?} {:?}->{:?}", s, i, o),
						},
						outputs.iter().map(Type::size_of).sum(),
						labels,
					),
					(inputs, outputs) => match (i.as_str(), inputs, outputs) {
						("call", _, _) => {
							let ret = *labels;
							*labels += 1;
							let inputs_size = inputs.iter().map(Type::size_of).sum();
							vec![IntPush(ret), Move(0, inputs_size, 8), Goto, Label(ret)]
						}
						("false", [], [Bool]) => vec![BoolPush(false)],
						("true", [], [Bool]) => vec![BoolPush(true)],
						("nop", [], []) => vec![],
						(s, i, o) => todo!("could not compile {:?} {:?}->{:?}", s, i, o),
					},
				}
			}
			Typed::Number(n) => vec![IntPush(*n)],
			Typed::Declaration(_) => vec![], // space is allocated in Block branch
			Typed::Assignment(_, _t) => todo!("move size of t bytes into allocated space"),
		}
	}
	compile(tree, &mut 0)
}

#[derive(Clone, Debug, PartialEq)]
struct Stack(Vec<u8>);

impl Stack {
	fn new() -> Stack {
		Stack(vec![])
	}

	fn push<A>(&mut self, value: A) {
		unsafe {
			self.0.extend(std::slice::from_raw_parts(
				&value as *const A as *const u8,
				std::mem::size_of::<A>(),
			))
		}
	}

	fn pop<A: Clone>(&mut self) -> A {
		unsafe {
			let data = self.0.split_off(self.0.len() - std::mem::size_of::<A>());
			(*data.as_ptr().cast::<A>()).clone()
		}
	}

	fn binary<A: Clone, B, F: Fn(A, A) -> B>(&mut self, f: F) {
		let (a, b) = (self.pop(), self.pop());
		self.push(f(a, b));
	}

	fn unary<A: Clone, B, F: Fn(A) -> B>(&mut self, f: F) {
		let a = self.pop();
		self.push(f(a));
	}
}

fn run(code: &[Operation]) -> Stack {
	let mut code = code.to_vec();
	code.extend([IntPush(-1), Move(0, 8, 8), Goto, Label(-1)]);

	let mut stack = Stack::new();
	let mut i = 0;
	while i < code.len() {
		match code[i] {
			IntPush(i) => stack.push(i),
			IntMul => stack.binary(|a: i64, b| a * b),
			IntAdd => stack.binary(|a: i64, b| a + b),
			IntNeg => stack.unary(|a: i64| -a),
			IntEq => stack.binary(|a: i64, b| a == b),
			IntLt => stack.binary(|a: i64, b| a < b),
			IntGt => stack.binary(|a: i64, b| a > b),
			BoolPush(b) => stack.push(b),
			BoolNot => stack.unary(|a: bool| !a),
			BoolAnd => stack.binary(|a: bool, b| a & b),
			BoolOr => stack.binary(|a: bool, b| a | b),
			BoolEq => stack.binary(|a: bool, b| a == b),
			Alloc(size) => stack.0.extend(std::iter::repeat(0u8).take(size)),
			Free(size) => stack.0.truncate(stack.0.len() - size),
			Move(start, end, size) => {
				let l = stack.0.len() - size;
				let data: Vec<u8> = stack.0.drain(l - start..l - start + size).collect();
				stack.0.splice(l - end..l - end, data);
			}
			Label(_) => {}
			Goto => {
				let a = stack.pop::<i64>();
				i = code
					.iter()
					.position(|o| matches!(o, Label(b) if a == *b))
					.unwrap_or_else(|| panic!("could not find label {}", a))
			}
		}
		i += 1;
	}
	stack
}
