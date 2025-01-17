use crate::*;

/// Matches `opener`s and `closer`s in the sibling list of the `head` node,
/// turning a flat list of tokens into a nested tree.
#[allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
pub fn bracket(_state: &mut State, _head: NodeRef) -> Result<()> {
    todo!()
}

#[test]
fn test() -> Result<()> {
    #[derive(Debug, PartialEq)]
    enum Tree {
        Leaf(String),
        Round(Vec<Tree>),
        Curly(Vec<Tree>),
        Square(Vec<Tree>),
        Root(Vec<Tree>),
    }

    macro_rules! node {
        [$leaf:ident] => { Tree::Leaf(stringify!($leaf).to_owned()) };
        [($($args:tt)*)] => { Tree::Round(vec![$(tree![$args]),*]) };
        [{$($args:tt)*}] => { Tree::Curly(vec![$(tree![$args]),*]) };
        [[$($args:tt)*]] => { Tree::Square(vec![$(tree![$args]),*]) };
    }

    macro_rules! tree {
        [$($args:tt)*] => { Tree::Root(vec![$(node!($args)),*]) }
    }

    fn state_to_tree(state: &State, node: usize) -> Tree {
        if let Some(head) = state.nodes[node].head.unpack() {
            let mut children = Vec::new();
            let mut child = NodeRef::some(head);
            while let Some(i) = child.unpack() {
                children.push(state_to_tree(state, i));
                child = state.nodes[i].next;
            }
            match state.nodes[node].span.string(state).chars().next() {
                Some('(') => Tree::Round(children),
                Some('{') => Tree::Curly(children),
                Some('[') => Tree::Square(children),
                None => Tree::Root(children),
                _ => panic!("non-bracket in {}", state.nodes[node].span.string(state)),
            }
        } else {
            Tree::Leaf(state.nodes[node].span.string(state).to_owned())
        }
    }

    fn run(text: &str) -> Result<Tree> {
        let name = String::new();
        let text = text.to_owned();
        let source = Source { name, text };
        let mut state = State::default();

        tokenize(&mut state, source, ROOT)?;

        let head = state.nodes[ROOT].head;
        bracket(&mut state, head)?;

        Ok(state_to_tree(&state, ROOT))
    }

    assert_eq!(run("()")?, tree!(()));
    assert_eq!(run("(()a)")?, tree!(() a));
    assert_eq!(run("a (b c) d")?, tree!(a (b c) d));
    assert_eq!(run("({[]}[()])")?, tree!(({ [] }[()])));
    assert_eq!(
        run("{{}").unwrap_err(),
        "error at :1:1: unmatched open bracket"
    );
    assert_eq!(
        run("[]][").unwrap_err(),
        "error at :1:3: unmatched close bracket"
    );
    assert_eq!(
        run("({)}").unwrap_err(),
        "error at :1:2: mismatched brackets `{` and `)`"
    );

    Ok(())
}
