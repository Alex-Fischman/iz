//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc)]

pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};

/// Global `Result` alias for ease of use.
pub type Result<T> = std::result::Result<T, String>;

/// A `Source` represents a unit of source code (for example, a `.iz` file).
pub struct Source {
    /// The location to use in error messages.
    pub name: String,
    /// The actual code, as an in-memory UTF-8 string.
    pub text: String,
}

/// Create a `Source` from a Rust `String`.
#[macro_export]
macro_rules! text {
    ($text:expr) => {
        Source {
            name: format!("({}:{}:{})", file!(), line!(), column!()),
            text: String::from($text),
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

/// A substring of `iz` code.
#[derive(Clone, Copy)]
pub struct Span {
    src: usize,
    idx: u32,
    len: u32,
}

impl Span {
    /// Get the substring that this `Span` corresponds to.
    #[must_use]
    pub fn string<'a>(&self, srcs: &'a [Source]) -> &'a str {
        &srcs[self.src].text[self.idx as usize..][..self.len as usize]
    }

    /// Get the location of the substring that this `Span` corresponds to.
    #[must_use]
    pub fn location(&self, srcs: &[Source]) -> String {
        let Source { name, text } = &srcs[self.src];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in text.char_indices() {
            match (i, c) {
                (i, _) if i == self.idx as usize => break,
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
