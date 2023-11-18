//! Parsing passes

use crate::*;

/// Parse matching bracket tokens
pub fn parse_brackets<'a>(open: &'a str, close: &'a str) -> Pass<'a> {
    Box::new(move |tree: &mut Tree, parent: Node| {
        tree.run_pass_over_children(parent, parse_brackets(open, close))?;

        let mut openers: Vec<(Node, usize)> = vec![];
        let mut i = 0;
        while i < tree.get_children(parent).len() {
            let child = tree.get_children(parent)[i];
            if tree.get_slice(child) == open {
                openers.push((child, i));
            } else if tree.get_slice(child) == close {
                let (opener_node, opener_index) = openers
                    .pop()
                    .ok_or(format!("extra {}", tree.get_printable(child)))?;

                let (lo, _) = tree.get_range(opener_node);
                let (_, hi) = tree.get_range(child);
                tree.set_range(opener_node, (lo, hi));

                tree.get_children_mut(parent).remove(i);
                let grandchildren: Vec<_> = tree
                    .get_children_mut(parent)
                    .drain(opener_index + 1..i)
                    .collect();
                tree.get_children_mut(opener_node).extend(grandchildren);
                i = opener_index;
            }
            i += 1;
        }
        if let Some((opener_node, _)) = openers.pop() {
            return Err(format!("extra {}", tree.get_printable(opener_node)));
        }

        Ok(())
    })
}
