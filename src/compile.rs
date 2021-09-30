use crate::bssem;
use crate::parse::Token;
use crate::tree::*;

pub fn compile(ast: Tree<Token>) -> Vec<bssem::I> {
    let mut program = ast.map(|t| Symbol::Tok(t.0, t.1));
    program.value = Symbol::Root;
    let mut function_id = 0;

    program.for_each(&mut |t| {
        if let Symbol::Tok(s, i) = &t.value {
            if let Ok(c) = s.parse() {
                t.value = Symbol::Code(vec![bssem::I::ADI(0, c, 3)], *i);
            }
        }
    });

    program.for_each(&mut |t| {
        if let Symbol::Tok(s, i) = &t.value {
            if s == "func" {
                t.value = Symbol::Fun(function_id, *i);
                if let Symbol::Tok(bound, _) = t.children[0].value.clone() {
                    t.children.remove(0);
                    t.for_each_with_index(
                        &mut |t, depth| {
                            if let Symbol::Tok(s, k) = &t.value {
                                if s == &bound {
                                    t.value = Symbol::Code(vec![bssem::I::LOA(2, -(depth as i64), 3)], *k);
                                }
                            }
                        },
                        0,
                    );
                }
                function_id += 1;
            }
        }
    });

    let mut funcs = Vec::new();
    program.for_each(&mut |t| {
        for c in t.children.iter_mut() {
            let mut is = Vec::new();
            for (i, gc) in c.children.iter().enumerate() {
                if let Symbol::Fun(_, _) = gc.value {
                    is.push(i);
                }
            }
            for i in is.iter().enumerate().map(|(i, f)| f - i) {
                if let Symbol::Fun(id, _) = c.children[i].value {
                    funcs.push(c.children.remove(i));
                    c.children.insert(i, Tree::new(Symbol::Ref(id)));
                }
            }
        }
    });
    program.children.splice(0..0, funcs);

    let mut program = program.map(|s| Environment {
        term: s,
        rules: Vec::new(),
    });

    program.for_each(&mut |t| {
        let mut rules = Vec::new();
        t.children.retain(|c| {
            if let Symbol::Tok(s, _i) = &c.value.term {
                if s == "set" {
                    if let Symbol::Tok(identifier, _j) = &c.children[0].value.term {
                        rules.push(Rule {
                            find: identifier.to_string(),
                            replace: c.children[1].map(|e| e.term)
                        });
                        return false;
                    } else {
                        panic!("set without identifier");
                    }
                }
            }
            true
        });
        t.value.rules = rules;
    });

    program.for_each(&mut |t| {
        let rules: Vec<Rule> = t.value.rules.drain(..).collect();
        t.for_each(&mut |c| {
            c.value.rules.extend(rules.iter().cloned());
        });
    });

    let mut dirty = true;
    while dirty {
        dirty = false;
        program.for_each(&mut |t| {
            if let Symbol::Tok(s, _i) = &t.value.term {
                if let Some(r) = t.value.rules.iter().find(|r| &r.find == s) {
                    dirty = true;
                    t.value.term = r.replace.value.clone();
                    t.children.splice(0..0, r.replace.map(|s| Environment {
                        term: s,
                        rules: t.value.rules.clone()
                    }).children);
                }
            }
        });
    }

    println!("{:?}", program);

    Vec::new()
}

#[derive(Clone)]
enum Symbol {
    Root,
    Code(Vec<bssem::I>, usize),
    Tok(String, usize),
    Fun(u64, usize),
    Ref(u64),
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Symbol::Root => write!(f, "Root"),
            Symbol::Code(c, _) => write!(f, "{:?}", c),
            Symbol::Tok(s, _) => write!(f, "tok {}", s),
            Symbol::Fun(i, _) => write!(f, "fun {}", i),
            Symbol::Ref(i) => write!(f, "ref {}", i),
        }
    }
}

#[derive(Clone)]
struct Environment {
    term: Symbol,
    rules: Vec<Rule>,
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.term)
        // write!(f, "{:?}: {:?}", self.term, self.rules)
    }
}

#[derive(Clone)]
struct Rule {
    find: String,
    replace: Tree<Symbol>,
}

impl std::fmt::Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} -> \n{:?}", self.find, self.replace)
    }
}
