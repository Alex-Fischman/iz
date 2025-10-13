//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc, clippy::too_many_lines)]

mod tokenize;

pub use tokenize::*;

pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};
pub use std::ops::Index;

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

/// The state of the compiler.
pub struct State {
    sources: Vec<Source>,
}

/// A handle to one `Source` in the `State`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SourceId(usize);

impl State {
    /// Create a new state. The provided source will have the returned `SourceId`.
    #[must_use]
    pub fn new(source: Source) -> (State, SourceId) {
        let mut state = State {
            sources: Vec::new(),
        };
        let src = state.add_source(source);
        (state, src)
    }

    /// Add a source to the state.
    pub fn add_source(&mut self, source: Source) -> SourceId {
        let src = SourceId(self.sources.len());
        self.sources.push(source);
        src
    }
}

impl Index<SourceId> for State {
    type Output = Source;
    fn index(&self, SourceId(src): SourceId) -> &Source {
        &self.sources[src]
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
