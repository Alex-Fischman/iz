//! The command line interface

#![deny(clippy::pedantic)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::type_complexity)]
#![allow(clippy::similar_names)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::must_use_candidate)]

pub mod tree;

pub use crate::tree::*;

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => eprintln!("{e}"),
    }
}

fn run() -> Result<(), String> {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {}", name))?;
    let source = Source { name, text };

    let mut tree = Tree::new_tree(&source);
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.new_child(ROOT, lo, hi);
    }

    for child in tree.get_children(ROOT) {
        println!("{}", tree.get_token(*child));
    }

    Ok(())
}
