//! A compiler for the `iz` programming language.

#![deny(clippy::all, clippy::pedantic, missing_docs)]
#![allow(clippy::missing_errors_doc)]

pub use std::collections::HashMap;
pub use std::fmt::{Debug, Display, Formatter};
use std::process::ExitCode;

/// Global `Result` alias for ease of use. See the `err!` macro.
pub type Result<T> = std::result::Result<T, String>;

/// Run the compiler, given the source code and an optional source name.
#[allow(clippy::needless_pass_by_value)]
pub fn run(text: String, name: Option<String>) -> Result<()> {
    if let Some(name) = name {
        println!("{name}\n");
    }
    println!("{text}");
    Ok(())
}

/// Run the compiler, given a source file name.
pub fn run_file(name: String) -> Result<()> {
    match std::fs::read_to_string(&name) {
        Ok(text) => run(text, Some(name)),
        Err(_) => Err(format!("could not read {name}")),
    }
}

/// Run the compiler, given the command line arguments.
pub fn run_args() -> Result<()> {
    let args: Vec<_> = std::env::args().collect();
    let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
    run_file(name)
}

fn main() -> ExitCode {
    match run_args() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_program_ok() -> Result<()> {
        run(String::new(), None)
    }
}
