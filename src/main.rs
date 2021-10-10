pub mod parse;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();
    let (program, operators) = parse::preprocess(&program);

    let program = parse::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));
    println!();

    let program = parse::parse(&program, &operators);
    println!("{:?}", program);
}
