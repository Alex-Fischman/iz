//! The command line interface

#![deny(missing_docs)]
#![deny(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::must_use_candidate)]

pub mod parse;
pub mod tree;

pub use tree::*;

fn main() {
    std::process::exit(match run() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}

fn run() -> Result {
    let args = std::env::args().collect::<Vec<String>>();
    let name = args.get(1).ok_or("usage: pass a .iz file")?.to_string();
    let text = std::fs::read_to_string(&name).map_err(|_| format!("could not read {name}"))?;
    let source = Source { name, text };

    let mut tree = Tree::new_tree(Slice::new(&source, 0, 0));
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.new_child(ROOT, Slice::new(&source, lo, hi));
    }

    parse::brackets("(", ")")(&mut tree, ROOT)?;

    for child in tree.get_children(ROOT) {
        println!("{}", tree.get_slice(*child));
    }

    Ok(())
}
