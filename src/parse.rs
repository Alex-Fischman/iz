//! Parsing passes

use crate::*;

/// Parse matching bracket tokens
pub fn parse_brackets<'a>(open: &'a str, close: &'a str) -> Pass<'a> {
    Box::new(|tree: &mut Tree, node: Node| {
        let children: Vec<Node> = tree.get_children(node).to_vec();
        for node in children {
            parse_brackets(open, close)(tree, node)?;
        }

        todo!()
    })
}
