//! The command line interface to the compiler.

use crate::pass::Passes;
use crate::token::{Source, Token};
use crate::tree::Tree;

mod pass;
mod token;
mod tree;

mod analysis;
mod codegen;
mod lexing;
mod parsing;

fn main() {
    let name = std::env::args()
        .collect::<Vec<String>>()
        .get(1)
        .unwrap_or_else(|| panic!("usage: pass a .iz file"))
        .to_string();
    let text = std::fs::read_to_string(&name).unwrap_or_else(|_| panic!("could not read {}", name));
    let source = std::rc::Rc::new(Source { name, text });

    let mut tree = Tree::new(Token {
        source: source.clone(),
        range: 0..0,
    });
    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        tree.children.push(Tree::new(Token {
            source: source.clone(),
            range: lo..hi,
        }));
    }

    let mut passes = Passes::default();
    passes.push(lexing::concat_strings);
    passes.push(lexing::remove_comments);
    passes.push(lexing::remove_whitespace);
    passes.push(lexing::concat_tokens(Token::is_identifier));
    passes.push(lexing::concat_tokens(Token::is_operator));
    passes.push(lexing::parse_integers);
    passes.push(lexing::parse_strings);
    passes.push(parsing::parse_brackets("(", ")"));
    passes.push(parsing::parse_brackets("{", "}"));
    passes.push(parsing::parse_brackets("[", "]"));
    passes.push(parsing::parse_postfixes(&[":", "?", "&"]));
    passes.push(analysis::annotate_intrinsics);
    passes.push(analysis::compute_types);
    passes.push(codegen::compile_intrinsics_x64);
    passes.push(codegen::compile_program_x64);
    passes.run(&mut tree);
}
