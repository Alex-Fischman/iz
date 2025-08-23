//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc)]

pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};

/// Global `Result` alias for ease of use.
pub type Result<T> = std::result::Result<T, String>;

/// Constructs an error message from the given source, span, and format arguments.
#[macro_export]
macro_rules! err {
    ($source:expr, $span:expr, $($fmt:tt)*) => {
        Err(format!(
            "error at {}: {}\n{}",
            $span.location($source),
            format!($($fmt)*),
            $span.string($source),
        ))
    };
}

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
            name: ::std::string::String::from("TEST"),
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

/// A substring of a `Source`.
#[derive(Clone, Copy)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    /// Get the substring that this `Span` corresponds to.
    #[must_use]
    pub fn string<'a>(&self, src: &'a Source) -> &'a str {
        &src.text[self.lo..self.hi]
    }

    /// Get the location of the substring that this `Span` corresponds to.
    #[must_use]
    pub fn location(&self, Source { name, text }: &Source) -> String {
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

#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum Side {
    Left,
    Right,
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq)]
pub enum Bracket {
    Paren,
    Curly,
    Square,
}

/// The different types of tokens in an `iz` program.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// One of `(`, `)`, `{`, `}`, `[`, or `]`.
    Bracket(Bracket, Side),
    /// A string, which has been parsed with escape codes.
    String(String),
    /// Everything else, which is whitespace-separated.
    Other,
}

impl Source {
    fn next_char(&self, idx: usize) -> Option<char> {
        self.text[idx..].chars().next()
    }

    fn skip_whitespace(&self, mut idx: usize) -> usize {
        loop {
            match self.next_char(idx) {
                Some('#') => loop {
                    match self.next_char(idx) {
                        None | Some('\n') => break,
                        Some(c) => idx += c.len_utf8(),
                    }
                },
                Some(c) if c.is_whitespace() => idx += c.len_utf8(),
                _ => return idx,
            }
        }
    }

    /// Get the token at byte position `idx` in the text.
    pub fn next_token(&self, idx: usize) -> Result<Option<(Span, Token)>> {
        let idx = self.skip_whitespace(idx);
        let Some(c) = self.next_char(idx) else {
            return Ok(None);
        };
        let _span = Span {
            lo: idx,
            hi: idx + c.len_utf8(),
        };

        todo!()
    }
}

/// A wrapper type around some bytes that should be interpreted as assembly code.
pub struct Assembly(pub Vec<u8>);

impl Assembly {
    /// Run an `Assembly` program.
    pub fn run(&self) -> Result<()> {
        todo!()
    }
}

/// Compile a `Source` down to `Assembly`.
pub fn compile(source: Source) -> Result<Assembly> {
    let sources = [source];

    eprintln!("{}\n{}", sources[0].name, sources[0].text);

    Ok(Assembly(Vec::new()))
}

fn main() -> std::process::ExitCode {
    match (|| {
        let args: Vec<_> = std::env::args().collect();
        let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
        let source = Source::from_file(name)?;
        let _assembly = compile(source)?;
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
        let assembly = compile(text!(""))?;
        assert!(assembly.0.is_empty());
        Ok(())
    }
}
