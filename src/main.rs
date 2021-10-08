pub mod parse;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();

    let (program, prefixes, affixes) = parse::preprocess(&program);
    println!("{:?}", prefixes);
    println!("{:?}", affixes);

    let program = parse::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));
    println!();

    let program = parse::parse(&program, &prefixes, &affixes);
    println!("{:?}", program);

    let s = parse::parse(&parse::tokenize("1"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "1");

    let s = parse::parse(&parse::tokenize("1 + 2 * 3"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(+ 1 (* 2 3))");

    let s = parse::parse(&parse::tokenize("a + b * c * d + e"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(+ (+ a (* (* b c) d)) e)");

    let s = parse::parse(&parse::tokenize("f . g . h"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(. (. f g) h)");

    let s = parse::parse(&parse::tokenize("f = g = h"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(= f (= g h))");

    let s = parse::parse(&parse::tokenize(" 1 + 2 + f = g = h * 3 * 4"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(= (+ (+ 1 2) f) (= g (* (* h 3) 4)))");

    let s = parse::parse(&parse::tokenize("- -1 * 2"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(* (- (- 1)) 2)");

    let s = parse::parse(&parse::tokenize("- -f . g"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(- (- (. f g)))");

    let s = parse::parse(&parse::tokenize("-9!"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(- (! 9))");

    let s = parse::parse(&parse::tokenize("f . g !"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(! (. f g))");

    let s = parse::parse(&parse::tokenize("(((0)))"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(( (( (( 0)))");

    let s = parse::parse(&parse::tokenize("x[0][1]"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "([ ([ x 0) 1)");

    let s = parse::parse(&parse::tokenize("if a; b; if c; d; e"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(if a b (if c d e))");

    let s = parse::parse(&parse::tokenize("a = if 0; b ; c = d"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(= a (= (if 0 b c) d))");
}
