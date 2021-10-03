pub mod parse;
pub mod token;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();

    let program = token::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));

    let program = parse::parse(&program);
    print!("{:?}", program);
}
