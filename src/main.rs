//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc, clippy::too_many_lines)]

mod bracket;
mod state;
mod tokenize;

pub use state::*;
pub use tokenize::*;

pub use std::any::Any;
pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};
pub use std::ops::{Index, IndexMut};

/// Global `Result` alias for ease of use.
pub type Result<T> = std::result::Result<T, String>;

/// A substring of a `Source` in a `State`.
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
    let (mut state, src) = State::new(source);
    let tokens = state.add_table::<Token>();
    state.tokenize(src, tokens, State::ROOT)?;
    state.bracket(tokens, State::ROOT)?;

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
