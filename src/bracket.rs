use crate::*;

/// Matches `opener`s and `closer`s in the children of the `parent` node,
/// turning a flat list of tokens into a nested tree.
#[allow(clippy::missing_errors_doc, clippy::missing_panics_doc)]
pub fn bracket(state: &mut State, parent: usize) -> Result<()> {
    let opener_tag = state.get_tag("opener")?;
    let closer_tag = state.get_tag("closer")?;

    let mut stack = Vec::new();
    let mut child = state.nodes[parent].head;

    while let Some(i) = child.unpack() {
        if state.nodes[i].tag == opener_tag {
            stack.push(i);
        } else if state.nodes[i].tag == closer_tag {
            let Some(opener) = stack.pop() else {
                return err!(state, state.nodes[i].span, "unmatched close bracket");
            };

            // set opener span to encompass the whole tree
            assert_eq!(state.nodes[opener].span.source, state.nodes[i].span.source);
            state.nodes[opener].span.hi = state.nodes[i].span.hi;

            // check that opener and closer are matched
            let closer = state.nodes[opener].data.downcast_ref::<&str>().unwrap();
            if state.nodes[i].span.string(state) != *closer {
                return err!(state, state.nodes[opener].span, "mismatched brackets");
            }

            // if there are any nodes between the brackets:
            if state.nodes[opener].next.unpack().unwrap() != i {
                let head = state.nodes[opener].next.unpack().unwrap();
                let last = state.nodes[i].prev.unpack().unwrap();

                // move contents into opener
                state.nodes[opener].head = NodeRef::some(head);
                state.nodes[opener].last = NodeRef::some(last);

                // break pointers out of contents
                state.nodes[head].prev = NodeRef::NONE;
                state.nodes[last].next = NodeRef::NONE;
            }

            // break pointers into contents
            state.nodes[opener].next = state.nodes[i].next;
            state.nodes[i].prev = state.nodes[opener].prev;

            // fix up parent's pointers
            if state.nodes[parent].last.unpack().unwrap() == i {
                state.nodes[parent].last = NodeRef::some(opener);
            }

            // fix up sibling's pointers
            if let Some(next) = state.nodes[i].next.unpack() {
                state.nodes[next].prev = NodeRef::some(opener);
            }
        }

        child = state.nodes[i].next;
    }

    if let Some(i) = stack.pop() {
        return err!(state, state.nodes[i].span, "unmatched open bracket");
    }

    Ok(())
}

#[test]
fn test() -> Result<()> {
    #[derive(Debug, PartialEq)]
    enum Tree {
        Leaf(String),
        Limb(LimbType, Vec<Tree>),
    }

    #[derive(Debug, PartialEq)]
    enum LimbType {
        Paren,
        Curly,
        Square,
        Root,
    }

    macro_rules! node {
        [$leaf:ident] => { Tree::Leaf(stringify!($leaf).to_owned()) };
        [($($args:tt)*)] => { Tree::Limb(LimbType::Paren, vec![$(node![$args]),*]) };
        [{$($args:tt)*}] => { Tree::Limb(LimbType::Curly, vec![$(node![$args]),*]) };
        [[$($args:tt)*]] => { Tree::Limb(LimbType::Square, vec![$(node![$args]),*]) };
    }

    macro_rules! tree {
        [$($args:tt)*] => { Tree::Limb(LimbType::Root, vec![$(node!($args)),*]) }
    }

    fn state_to_tree(state: &State, parent: usize) -> Tree {
        let limb_type = match state.nodes[parent].span.string(state).chars().next() {
            None => LimbType::Root,
            Some('(') => LimbType::Paren,
            Some('{') => LimbType::Curly,
            Some('[') => LimbType::Square,
            Some(_) => return Tree::Leaf(state.nodes[parent].span.string(state).to_owned()),
        };

        let mut children = Vec::new();
        let mut child = state.nodes[parent].head;
        while let Some(i) = child.unpack() {
            children.push(state_to_tree(state, i));
            child = state.nodes[i].next;
        }
        Tree::Limb(limb_type, children)
    }

    fn run(text: &str) -> Result<Tree> {
        let name = String::new();
        let text = text.to_owned();
        let source = Source { name, text };
        let mut state = State::default();
        tokenize(&mut state, source, ROOT)?;
        bracket(&mut state, ROOT)?;
        Ok(state_to_tree(&state, ROOT))
    }

    assert_eq!(run("a")?, tree!(a));
    assert_eq!(run("()")?, tree!(()));
    assert_eq!(run("()a")?, tree!(() a));
    assert_eq!(run("(()a)")?, tree!((() a)));
    assert_eq!(run("a (b c) d")?, tree!(a (b c) d));
    assert_eq!(run("(y eq (map f xs))")?, tree!((y eq (map f xs))));
    assert_eq!(run("({[]}[()])")?, tree!(({ [] }[()])));
    assert_eq!(
        run("{{}").unwrap_err(),
        "error at :1:1: unmatched open bracket\n{"
    );
    assert_eq!(
        run("[]][").unwrap_err(),
        "error at :1:3: unmatched close bracket\n]"
    );
    assert_eq!(
        run("({)}").unwrap_err(),
        "error at :1:2: mismatched brackets\n{)"
    );

    Ok(())
}
