#![no_implicit_prelude]
#![allow(clippy::print_with_newline)]
extern crate std;

use std::{
    env::args,
    fs::read_to_string,
    iter::Iterator,
    result::{Result, Result::Ok},
    string::String,
    vec::Vec,
    {format, print},
};

fn main() -> Result<(), String> {
    let args: Vec<String> = args().collect();
    let file = args
        .get(1)
        .ok_or("pass a .iz file as a command line argument")?;
    let text = read_to_string(file).map_err(|_| format!("could not read {}", file))?;

    print!("{}\n", text);

    Ok(())
}
