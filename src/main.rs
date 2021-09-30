mod bssem;
mod compile;
mod parse;
mod tree;

fn main() {
    let ast = parse::parse(&std::fs::read_to_string("scratch.iz").unwrap());
    let _program = compile::compile(ast);
}
