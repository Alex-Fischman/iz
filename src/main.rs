pub mod parse;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();

    let (program, operators) = parse::preprocess(&program);
    println!("{:?}", operators);
    println!("{:?}", program);

    let program = parse::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));
    println!();

    let program = parse::parse(&program, &operators);
    println!("{:?}", program);
}

#[test]
fn tokenization() {
    let test = |a, b| assert_eq!(format!("{:?}", parse::tokenize(a)), b);
    test(
        "asdf asdf\t\n fj + ++ + ->a -a2b",
        "[asdf, asdf, fj, +, ++, +, ->, a, -, a, 2, b]",
    );
}

#[test]
fn parsing() {
    let ops = std::collections::HashMap::new();
    let test = |a, b| assert_eq!(format!("{:?}", parse::parse(&parse::tokenize(a), &ops)), b);

    test("1", "({ 1)");
    test("a + b", "({ (add a b))");
    test("a + b + c", "({ (add (add a b) c))");
    test("a = b = c", "({ (set a (set b c)))");
    test("a + b * c * d + e", "({ (add (add a (mul (mul b c) d)) e))");
    test("1 + f = g = h * 3", "({ (set (add 1 f) (set g (mul h 3))))");
    test("if a b", "({ (if_ a b))");
    test("- -1 * 2", "({ (mul (neg (neg 1)) 2))");
    test("- -f . g", "({ (neg (neg (dot f g))))");
    test("- !b", "({ (neg (not b)))");
    test("! f . g", "({ (not (dot f g)))");
    test("(((0)))", "({ (( (( (( 0))))");
    // test("x[0][1]", "({ (idx (idx x 0) 1))");
    test(
        "if a b else if c d else e",
        "({ (else_ (if_ a b) (else_ (if_ c d) e)))",
    );
    test(
        "a = if 0 b else c = d",
        "({ (set a (set (else_ (if_ 0 b) c) d)))",
    );
}
