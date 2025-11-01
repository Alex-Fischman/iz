//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc, clippy::too_many_lines)]

mod tokenize;

pub use tokenize::*;

pub use std::any::Any;
pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};
pub use std::ops::{Index, IndexMut};

/// Global `Result` alias for ease of use.
pub type Result<T> = std::result::Result<T, String>;

/// A `Source` represents a unit of source code (for example, a `.iz` file).
pub struct Source {
    /// The location to use in error messages.
    pub name: String,
    /// The actual code, as an in-memory UTF-8 string.
    pub text: String,
}

/// Create a `Source` from a Rust `String` for testing.
#[macro_export]
macro_rules! text {
    ($text:expr) => {
        Source {
            name: ::std::string::String::from("text!"),
            text: ::std::string::String::from($text),
        }
    };
}

impl Source {
    /// Create a `Source` by reading a file.
    pub fn from_file(name: String) -> Result<Source> {
        match std::fs::read_to_string(&name) {
            Ok(text) => Ok(Source { name, text }),
            Err(_) => Err(format!("could not read {name}")),
        }
    }
}

/// A handle to one `Source` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceId(usize);

/// A handle to one `Node` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

/// A handle to one `Table<T>` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TableId<T>(usize, std::marker::PhantomData<T>);

/// A `Node` represents one element of the program that is being compiled.
pub struct Node {
    children: Vec<NodeId>,
}

/// A `Table` is used to store one type of information for `Node`s.
pub struct Table<T>(HashMap<NodeId, T>);

/// The state of the compiler.
pub struct State {
    sources: Vec<Source>,
    nodes: Vec<Node>,
    tables: Vec<Box<dyn Any>>,
}

impl State {
    /// Create a new state. The provided source will have the returned `SourceId`.
    #[must_use]
    pub fn new(source: Source) -> (State, SourceId) {
        let mut state = State {
            sources: Vec::new(),
            nodes: Vec::new(),
            tables: Vec::new(),
        };
        let src = state.add_source(source);
        (state, src)
    }

    /// Add a `Source` to the `State`.
    pub fn add_source(&mut self, source: Source) -> SourceId {
        let src = SourceId(self.sources.len());
        self.sources.push(source);
        src
    }

    /// Add a new leaf `Node` to the `State`.
    pub fn add_node(&mut self, parent: Option<NodeId>) -> NodeId {
        let node = NodeId(self.nodes.len());
        self.nodes.push(Node {
            children: Vec::new(),
        });
        if let Some(parent) = parent {
            self[parent].children.push(node);
        }
        node
    }

    /// Add a new empty `Table` to the `State`.
    pub fn add_table<T: 'static>(&mut self) -> TableId<T> {
        let tbl = TableId(self.tables.len(), std::marker::PhantomData);
        self.tables.push(Box::new(Table::<T>(HashMap::new())));
        tbl
    }
}

impl Index<SourceId> for State {
    type Output = Source;
    fn index(&self, SourceId(src): SourceId) -> &Source {
        &self.sources[src]
    }
}

impl Index<NodeId> for State {
    type Output = Node;
    fn index(&self, NodeId(node): NodeId) -> &Node {
        &self.nodes[node]
    }
}

impl IndexMut<NodeId> for State {
    fn index_mut(&mut self, NodeId(node): NodeId) -> &mut Node {
        &mut self.nodes[node]
    }
}

impl<T: 'static> Index<TableId<T>> for State {
    type Output = Table<T>;
    fn index(&self, TableId(tbl, _): TableId<T>) -> &Table<T> {
        self.tables[tbl].downcast_ref().unwrap()
    }
}

impl<T: 'static> IndexMut<TableId<T>> for State {
    fn index_mut(&mut self, TableId(tbl, _): TableId<T>) -> &mut Table<T> {
        self.tables[tbl].downcast_mut().unwrap()
    }
}

/// A custom `Iterator` trait that can modify `State` and returns a `Result`.
pub trait Iterator<T> {
    /// Get the next element of the iterator.
    /// If we're iterating over a tree, this should be a postorder traversal.
    fn next(&mut self, state: &mut State) -> Result<Option<T>>;
}

/// A substring of a `Source` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    src: SourceId,
    lo: usize,
    hi: usize,
}

impl Span {
    /// Get the substring that this `Span` corresponds to.
    #[must_use]
    pub fn string<'a>(&self, state: &'a State) -> &'a str {
        &state[self.src].text[self.lo..self.hi]
    }

    /// Get the location of this `Span`.
    #[must_use]
    pub fn location(&self, state: &State) -> String {
        let Source { name, text } = &state[self.src];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!("{name}:{row}:{col}")
    }
}

/// Constructs an error message from the given state, span, and format arguments.
#[macro_export]
macro_rules! err {
    ($state:expr, $span:expr, $($fmt:tt)*) => {
        Err(format!(
            "error at {}: {}\n{}",
            $span.location($state),
            format!($($fmt)*),
            $span.string($state),
        ))
    };
}

/// One instruction in the intermediate representation.
pub enum Instruction {}

/// Compile a `Source` down to `Instruction`s.
pub fn compile(source: Source) -> Result<Vec<String>> {
    let (state, src) = State::new(source);

    eprintln!("{}\n{}", state[src].name, state[src].text);

    Ok(Vec::new())
}

fn main() -> std::process::ExitCode {
    match (|| {
        let args: Vec<_> = std::env::args().collect();
        let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
        let source = Source::from_file(name)?;
        let _instructions = compile(source)?;
        Ok::<(), String>(())
    })() {
        Ok(()) => std::process::ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            std::process::ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_program_ok() -> Result<()> {
        let instructions = compile(text!(""))?;
        assert!(instructions.is_empty());
        Ok(())
    }
}
