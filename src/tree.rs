//! The intermediate representation

/// Represents the source code of an iz program, e.g. a .iz file
pub struct Source {
    /// The name of the source, e.g. a file name
    pub name: String,
    /// The text of the source, e.g. the file's contents
    pub text: String,
}

/// Represents one part of the source code of an iz program
#[derive(Clone, Copy)]
pub struct Span<'a> {
    /// A reference to the `Source`
    pub source: &'a Source,
    /// The byte index of the start of the span
    pub lo: usize,
    /// The byte index of the end of the span (exclusive)
    pub hi: usize,
}

impl<'a> Span<'a> {
    /// Create a new span
    pub fn new(source: &Source, lo: usize, hi: usize) -> Span {
        Span { source, lo, hi }
    }

    /// Get the string that this span represents
    pub fn str(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

impl<'a> std::fmt::Display for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut row = 1;
        let mut col = 1;
        for (i, c) in self.source.text.char_indices() {
            match (i, c) {
                (i, _) if i == self.lo => break,
                (_, '\n') => {
                    row += 1;
                    col = 1;
                }
                _ => col += 1,
            }
        }
        write!(f, "{:?} at {}:{row}:{col}", self.str(), self.source.name)
    }
}

/// An identifier for a node of a `Tree`
#[derive(Clone, Copy)]
pub struct Node(usize);

struct Annotation<T>(Vec<T>);

impl<T> Annotation<T> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn push(&mut self, x: T) {
        self.0.push(x);
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

/// The intermediate representation of a program
pub struct Tree<'a> {
    spans: Annotation<Span<'a>>,
    children: Annotation<Vec<Node>>,
}

/// The identifier for the root node of any `Tree`
pub const ROOT: Node = Node(0);

#[allow(clippy::type_complexity)] // can't factor out the trait from Pass

/// The type of a compiler pass
pub struct Pass<'a>(Box<dyn Fn(&mut Tree, Node) -> Result + 'a>);

/// An specialized alias for the return value of compiler passes
pub type Result = std::result::Result<(), String>;

impl<'a> Tree<'a> {
    /// Create a new, empty `Tree`
    pub fn new_tree(span: Span) -> Tree {
        Tree {
            spans: Annotation(vec![span]),
            children: Annotation(vec![vec![]]),
        }
    }

    /// Create a new child node
    pub fn new_child(&mut self, parent: Node, span: Span<'a>) -> Node {
        let child = Node(self.spans.len());
        self.spans.push(span);
        self.children.push(vec![]);
        self.children[parent].push(child);
        child
    }

    /// Get the children of a node
    pub fn get_children(&self, parent: Node) -> &[Node] {
        &self.children[parent]
    }

    /// Get a mutable vector of the children of a node
    pub fn get_children_mut(&mut self, parent: Node) -> &mut Vec<Node> {
        &mut self.children[parent]
    }

    /// Get the `Span` of a `Node`
    pub fn get_span(&self, node: Node) -> Span<'a> {
        self.spans[node]
    }

    /// Set the `Span` of a `Node`
    pub fn set_span(&mut self, node: Node, span: Span<'a>) {
        self.spans[node] = span;
    }

    /// Run a `Pass` over all the children of a `Node`
    pub fn run_pass(&mut self, parent: Node, pass: &Pass) -> Result {
        let children = self.get_children(parent).to_vec();
        for &child in &children {
            self.run_pass(child, pass)?;
            pass.0(self, child)?;
        }
        Ok(())
    }
}

impl<'a> Pass<'a> {
    /// Wrap a `Fn(&mut Tree, Node) -> Result` into a `Pass`
    pub fn new(func: impl Fn(&mut Tree, Node) -> Result + 'a) -> Pass<'a> {
        Pass(Box::new(func))
    }
}
