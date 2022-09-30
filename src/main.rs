mod analyze;
mod interpret;
mod parse;
mod tokenize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Location(pub usize, pub usize);
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error(String, Location);

fn main() {
	match (|| {
		let args = std::env::args().collect::<Vec<String>>();
		let file = args.get(1).ok_or_else(|| "no file passed".to_owned())?;
		let chars: Vec<char> = std::fs::read_to_string(file)
			.map_err(|_| format!("could not read {}", file))?
			.chars()
			.collect();
		match (|| {
			let tokens = tokenize::tokenize(&chars)?;
			let trees = parse::parse(&tokens)?;
			let (trees, types) = analyze::analyze(&trees)?;
			interpret::interpret(&trees, &types)
		})() {
			Ok(values) => Ok(values),
			Err(Error(message, location)) => Err(if location.0 == 0 && location.1 == 0 {
				message
			} else {
				let fold = |(row, col), &c| match c {
					'\n' => (row + 1, 1),
					_ => (row, col + 1),
				};
				let (row0, col0) = chars.iter().take(location.0).fold((1, 1), fold);
				let (row1, col1) =
					chars.iter().skip(location.0).take(location.1).fold((row0, col0), fold);
				format!("{}\n{}:{}-{}:{}", message, row0, col0, row1, col1)
			}),
		}
	})() {
		Ok(vs) => println!("\n{:#?}", vs),
		Err(s) => println!("\nError: {}", s),
	}
}

const PRELUDE_PATH: &str = "/home/alex/Programming/iz/examples/prelude.iz";

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
#[derive(Clone, Debug, PartialEq)]
pub struct Context<K: Clone + Eq + Hash, V: Clone>(Rc<RefCell<ContextData<K, V>>>);
type ContextData<K, V> = (Option<Context<K, V>>, HashMap<K, V>);

impl<K: Clone + Eq + Hash, V: Clone> Context<K, V> {
	pub fn new(parent: Option<Context<K, V>>) -> Context<K, V> {
		Context(Rc::new(RefCell::new((parent, HashMap::new()))))
	}

	pub fn get<Q: Eq + Hash + ?Sized>(&self, key: &Q) -> Option<V>
	where
		K: std::borrow::Borrow<Q>,
		V: Clone,
	{
		match self.0.borrow().1.get(key) {
			Some(value) => Some(value.clone()),
			None => match &self.0.borrow().0 {
				None => None,
				Some(parent) => parent.get(key),
			},
		}
	}

	pub fn set(&self, key: K, value: V) -> Option<V> {
		fn f<K: Clone + Eq + Hash, V: Clone>(
			context: &Context<K, V>,
			key: &K,
		) -> Option<Context<K, V>> {
			match context.0.borrow().1.contains_key(key) {
				true => Some(context.clone()),
				false => match &context.0.borrow().0 {
					None => None,
					Some(parent) => f(parent, key),
				},
			}
		}
		f(self, &key).unwrap_or_else(|| self.clone()).0.borrow_mut().1.insert(key, value)
	}
}
