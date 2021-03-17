use std::cell::RefCell;
use std::rc::Rc;

fn main() {
    let tokens: Vec<Token> = std::fs::read_to_string("scratch.iz")
        .expect("file")
        .chars()
        .enumerate()
        .map(|(i, c)| Token(c.to_string(), i))
        .collect();

    let mut temp: Vec<Token> = Vec::new();
    let mut in_string = false;
    let mut in_escape = false;
    for t in tokens {
        if in_string {
            if in_escape {
                in_escape = false;
                if t.0 == "n" {
                    temp.last_mut().unwrap().0.push_str("\n");
                } else if t.0 == "t" {
                    temp.last_mut().unwrap().0.push_str("\t");
                } else if t.0 == "r" {
                    temp.last_mut().unwrap().0.push_str("\r");
                } else {
                    temp.last_mut().unwrap().0.push_str(&t.0);
                }
            } else if t.0 == "\\" {
                in_escape = true;
            } else {
                temp.last_mut().unwrap().0.push_str(&t.0);
                if t.0 == "\"" {
                    in_string = false;
                }
            }
        } else if t.0 == "\"" {
            in_string = true;
            temp.push(t);
        } else {
            temp.push(t.clone());
        }
    }
    let tokens = temp;

    let mut temp = Vec::new();
    let mut comments = Vec::new();
    let mut in_comment = false;
    for t in tokens {
        if t.0 == "#" && !in_comment {
            in_comment = true;
            comments.push(t);
        } else if t.0 == "\n" && in_comment {
            in_comment = false;
        } else if in_comment {
            comments.last_mut().unwrap().0.push_str(&t.0);
        } else {
            temp.push(t);
        }
    }
    let tokens = temp;

    let mut affixes: Vec<Affix> = Vec::new();
    let mut matches: Vec<Match> = Vec::new();
    for t in comments {
        if t.0.starts_with("#match") {
            let args: Vec<&str> = t.0.split(" ").filter(|s| !s.is_empty()).collect();
            matches.push(Match {
                opener: args[1].to_string(),
                closer: args[2].to_string(),
                func: args[3].to_string(),
            });
        } else if t.0.starts_with("#affix") {
            let args: Vec<&str> = t.0.split(" ").filter(|s| !s.is_empty()).collect();
            affixes.push(Affix {
                name: args[1].to_string(),
                func: args[2].to_string(),
                arity: args[3].parse().unwrap(),
                pos: args[4].parse().unwrap(),
                prec: args[5].parse().unwrap(),
                assoc: if args[6].to_string() == "l" { Assoc::Left} else if args[6].to_string() == "r" {Assoc::Right} else {panic!("invalid assoc: {}", args[6].to_string())},
            });
        }
    }
    affixes.sort_unstable_by_key(|a| a.prec);
    let tokens: Vec<Token> = tokens
        .split(|t| t.0.chars().next().unwrap().is_whitespace())
        .filter(|v| !v.is_empty())
        .map(|v| {
            Token(
                v.iter()
                    .map(|t| t.0.clone())
                    .collect::<Vec<String>>()
                    .join(""),
                v[0].1,
            )
        })
        .collect();

    let mut names: Vec<&str> = matches
        .iter()
        .flat_map(|m| vec![m.opener.as_str(), m.closer.as_str()])
        .chain(affixes.iter().map(|a| a.name.as_str()))
        .collect();
    names.sort_unstable_by_key(|s| s.len() as isize * -1);
    let tokens: Vec<Token> = tokens
        .iter()
        .flat_map(|t| {
            names.iter().fold(vec![t.clone()], |ts, n| {
                ts.iter()
                    .flat_map(|t| {
                        let mut ts = Vec::new();
                        if t.0.starts_with("\"")
                            || n.chars().all(|c| c.is_alphabetic())
                            || names.contains(&&t.0.as_str())
                        {
                            ts.push(t.clone());
                        } else {
                            let mut i = 0;
                            for (j, m) in t.0.match_indices(n) {
                                ts.push(Token(t.0[i..j].to_string(), t.1 + i));
                                ts.push(Token(m.to_string(), t.1 + j));
                                i = j + n.len();
                            }
                            ts.push(Token(t.0[i..].to_string(), i));
                        }
                        return ts;
                    })
                    .collect()
            })
        })
        .filter(|t| !t.0.is_empty())
        .collect();

    let ast = new_tree(Token("ROOT".to_string(), 0));
    let mut stack: Vec<&str> = vec![];
    for t in tokens {
        if &t.0 == stack.last().unwrap_or(&"") {
            stack.pop();
        } else {
            let mut tree = ast.clone();
            for _ in &stack {
                let child = Rc::clone(tree.borrow().children.last().unwrap());
                tree = child;
            }
            tree.borrow_mut().children.push(new_tree(t.clone()));

            if let Some(m) = matches.iter().find(|m| m.opener == t.0) {
                stack.push(&m.closer);
            }
        }
    }

    let ast = ast.map(|ast| {
        let ops = RefCell::new(Vec::new());
        for (i, c) in ast.children.iter().enumerate() {
            for a in &affixes {
                if c.borrow().value.0 == a.name {
                    ops.borrow_mut().push((i, a));
                }
            }
        }
        ops.borrow_mut().sort_by(|(i, a), (j, b)| {
            if a.prec == b.prec {
                if j < i && a.assoc == Assoc::Left {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }
            } else if a.prec < b.prec {
                std::cmp::Ordering::Greater
            } else {
                std::cmp::Ordering::Less
            }
        });
        let mut out = ast;
        let mut i = 0;
        while i < ops.borrow().len() {
            let start = ops.borrow()[i].0 - ops.borrow()[i].1.pos;
            let mut end = start + ops.borrow()[i].1.arity;

            let mut j = start;
            while j <= end {
                if ops.borrow()[i].0 != j {
                    let child = out.children.remove(j);
                    for (k, p) in ops.borrow_mut().iter_mut().enumerate() {
                        if k >= i && p.0 >= j {
                            p.0 -= 1;
                        }
                    }
                    out.children[ops.borrow()[i].0].borrow_mut().children.push(child);
                    end -= 1;
                } else {
                    j += 1;
                }
            }
            i += 1;
        }
        out
    });

    let ast = ast.map(|mut t| {
        for a in &affixes {
            if a.name == t.value.0 {
                t.value.0 = a.func.clone();
            }
        }
        for m in &matches {
            if m.opener == t.value.0 {
                t.value.0 = m.func.clone();
            }
        }
        t
    });

    print_tree(ast, 0);
}

#[derive(Clone, Debug)]
struct Token(String, usize);

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
struct Affix {
    name: String,
    func: String,
    arity: usize,
    pos: usize,
    prec: isize,
    assoc: Assoc,
}

#[derive(Debug, PartialEq)]
enum Assoc {Left, Right}

#[derive(Debug)]
struct Match {
    opener: String,
    closer: String,
    func: String,
}

#[derive(Debug)]
struct Node<T> {
    value: T,
    children: Vec<Tree<T>>,
}

type Tree<T> = Rc<RefCell<Node<T>>>;

fn new_tree<T>(v: T) -> Tree<T> {
    Rc::new(RefCell::new(Node {
        value: v,
        children: Vec::new()
    }))
}

fn print_tree<T>(t: Tree<T>, depth: usize)
where
    T: std::fmt::Display,
{
    println!("{}|{}", "-".repeat(depth), t.borrow().value.to_string());
    for c in &t.borrow().children {
        print_tree(Rc::clone(c), depth + 1);
    }
}

trait MappableTree<T> {
    fn map<F>(&self, f: F) -> Tree<T> where F: FnMut(Node<T>) -> Node<T> + Copy;
}

impl<T: Clone> MappableTree<T> for Tree<T> {
    fn map<F>(&self, mut f: F) -> Tree<T> where F: FnMut(Node<T>) -> Node<T> + Copy {
        let mut new = Node {
            value: self.borrow().value.clone(),
            children: Vec::new()
        };
        for c in &self.borrow().children {
            new.children.push(c.map(f));
        }
        Rc::new(RefCell::new(f(new)))
    }
}
