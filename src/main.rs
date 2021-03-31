#![allow(dead_code, unused_variables, unused_mut)]

mod bssem;
mod parse;
mod tree;

fn main() {
    let ast = parse::parse(&std::fs::read_to_string("scratch.iz").unwrap());

    let mut program = ast.map(|t| Symbol::Tok(t.0, t.1));
    program.value = Symbol::Root;
    let mut function_id = 0;

    program.for_each(&mut |t| {
        if let Symbol::Tok(s, i) = &t.value {
            if let Ok(c) = s.parse() {
                t.value = Symbol::Int(c, *i);
            }
        }
    });

    program.for_each(&mut |t| {
        if let Symbol::Tok(s, i) = &t.value {
            if s == "func" {
                t.value = Symbol::Fun(function_id, *i);
                if let Symbol::Tok(bound, j) = t.children[0].value.clone() {
                	t.children.remove(0);
                	t.for_each(&mut |t| {
                		if let Symbol::Tok(s, k) = &t.value {
                			if s == &bound {
                				t.value = Symbol::Arg(function_id, *k);
                			}
                		}
                	});
                }
                function_id += 1;
            }
        }
    });

    println!("{:?}", program);
}

#[derive(Clone)]
enum Symbol {
    Root,
    Code(Vec<bssem::I>),
    Tok(String, usize),
    Int(i64, usize),
    Fun(u64, usize),
    Arg(u64, usize),
    Cal(u64, usize),
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    	match self {
    		Symbol::Root => write!(f, "Root"),
    		Symbol::Code(c) => write!(f, "{:?}", c),
		    Symbol::Tok(s, _) => write!(f, "tok {}", s),
		    Symbol::Int(i, _) => write!(f, "int {}", i),
		    Symbol::Fun(i, _) => write!(f, "fun {}", i),
		    Symbol::Arg(i, _) => write!(f, "arg {}", i),
		    Symbol::Cal(i, _) => write!(f, "cal {}", i),
    	}
    }
}
