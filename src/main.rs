//! The command line interface for iz

#![deny(clippy::pedantic)]
#![deny(missing_docs)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::type_complexity)]
#![allow(clippy::similar_names)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::must_use_candidate)]

struct Source {
    name: String,
    text: String,
}

/// A non-iterator range that is Copy
#[derive(Clone, Copy)]
struct Range<T> {
    lo: T,
    hi: T,
}

#[derive(Clone, Copy)]
struct Node(usize);

struct Annotation<T>(Vec<T>);

impl<T> Annotation<T> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn push(&mut self, x: T) {
        self.0.push(x)
    }
}

impl<T> std::ops::Index<Node> for Annotation<T> {
    type Output = T;
    fn index(&self, Node(i): Node) -> &T {
        &self.0[i]
    }
}

impl<T> std::ops::IndexMut<Node> for Annotation<T> {
    fn index_mut(&mut self, Node(i): Node) -> &mut T {
        &mut self.0[i]
    }
}

struct Tree<'a> {
    source: &'a Source,
    ranges: Annotation<Range<usize>>,
    children: Annotation<Vec<Node>>,
}

const ROOT: Node = Node(0);

impl<'a> Tree<'a> {
    fn new_tree(source: &Source) -> Tree {
        Tree {
            source,
            ranges: Annotation(vec![Range { lo: 0, hi: 0 }]),
            children: Annotation(vec![vec![]]),
        }
    }

    fn new_child(&mut self, parent: Node, lo: usize, hi: usize) -> Node {
        let child = Node(self.ranges.len());
        self.ranges.push(Range { lo, hi });
        self.children.push(vec![]);
        self.children[parent].push(child);
        child
    }

    fn get_children(&self, parent: Node) -> &[Node] {
        &self.children[parent]
    }

    fn get_slice(&self, node: Node) -> &str {
        &self.source.text[self.ranges[node].lo..self.ranges[node].hi]
    }

    fn get_token(&self, node: Node) -> String {
        let range = self.ranges[node];
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == range.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        format!(
            "{:?} at {}:{row}:{col}",
            self.get_slice(node),
            self.source.name
        )
    }
}

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
