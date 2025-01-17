//! A compiler for the Iz programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]

pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};

mod bracket;
mod state;
mod tokenize;
pub use bracket::*;
pub use state::*;
pub use tokenize::*;

/// Global `Result` alias for ease of use. See the `err!` macro.
pub type Result<T> = std::result::Result<T, String>;

/// Constructs an error message from the given state, span,
/// and format arguments.
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

fn main() {
    std::process::exit(match cmd() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}

fn cmd() -> Result<()> {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
    run(Source { name, text })
}

/// Represents a file containing source code.
pub struct Source {
    /// The file name.
    pub name: String,
    /// The file contents.
    pub text: String,
}

/// A snippet of a `Source`.
#[derive(Clone, Copy)]
pub struct Span {
    /// Which source file this span refers to.
    pub source: usize,
    /// The byte index of the first character in the snippet.
    pub lo: usize,
    /// The byte index after the last character in the snippet.
    pub hi: usize,
}

impl Span {
    /// Get the text from the snippet.
    #[must_use]
    pub fn string(self, state: &State) -> &str {
        &state.sources[self.source].text[self.lo..self.hi]
    }

    /// Assert that the snippet is a single `char` and return it.
    /// # Panics
    /// Will panic if this `Span` contains more or less than 1 `char`.
    #[must_use]
    pub fn single_char(self, state: &State) -> char {
        assert_eq!(self.string(state).chars().count(), 1);
        self.string(state).chars().next().unwrap()
    }

    /// Get the location of the snippet.
    #[must_use]
    pub fn location(&self, state: &State) -> String {
        let source = &state.sources[self.source];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!("{}:{row}:{col}", source.name)
    }
}

fn run(source: Source) -> Result<()> {
    let mut state = State::default();
    tokenize(&mut state, source, ROOT)?;
    bracket(&mut state, ROOT)?;

    let mut child = state.nodes[ROOT].head;
    while let Some(i) = child.unpack() {
        let span = state.nodes[i].span;
        println!("{}\t{}", span.location(&state), span.string(&state));
        child = state.nodes[i].next;
    }

    Ok(())
}
