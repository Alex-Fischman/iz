pub mod parse;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();

    let (program, prefixes, affixes) = parse::preprocess(&program);
    println!("{:?} {:?}", prefixes, affixes);
    println!("{:?}", program);

    let program = parse::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));
    println!();

    let program = parse::parse(&program, &prefixes, &affixes);
    println!("{:?}", program);

    let s = parse::parse(&parse::tokenize("1"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "1");

    let s = parse::parse(&parse::tokenize("1 + 2 * 3"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(add 1 (mul 2 3))");

    let s = parse::parse(&parse::tokenize("a + b * c * d + e"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(add (add a (mul (mul b c) d)) e)");

    let s = parse::parse(&parse::tokenize("f . g . h"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(dot (dot f g) h)");

    let s = parse::parse(&parse::tokenize("f = g = h"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(eq f (eq g h))");

    let s = parse::parse(
        &parse::tokenize(" 1 + 2 + f = g = h * 3 * 4"),
        &prefixes,
        &affixes,
    );
    assert_eq!(
        format!("{:?}", s),
        "(eq (add (add 1 2) f) (eq g (mul (mul h 3) 4)))"
    );

    let s = parse::parse(&parse::tokenize("- -1 * 2"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(mul (neg (neg 1)) 2)");

    let s = parse::parse(&parse::tokenize("- -f . g"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(neg (neg (dot f g)))");

    let s = parse::parse(&parse::tokenize("-9!"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(neg (fac 9))");

    let s = parse::parse(&parse::tokenize("f . g !"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(fac (dot f g))");

    let s = parse::parse(&parse::tokenize("(((0)))"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(( (( (( 0)))");

    let s = parse::parse(&parse::tokenize("x[0][1]"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(idx (idx x 0) 1)");

    let s = parse::parse(&parse::tokenize("if a; b; if c; d; e"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(if_ a b (if_ c d e))");

    let s = parse::parse(&parse::tokenize("a = if 0; b ; c = d"), &prefixes, &affixes);
    assert_eq!(format!("{:?}", s), "(eq a (eq (if_ 0 b c) d))");
}
