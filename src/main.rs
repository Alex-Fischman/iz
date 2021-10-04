pub mod parse;
pub mod token;

fn main() {
    let program = std::fs::read_to_string("scratch.iz").unwrap();

    let program = token::tokenize(&program);
    program.iter().for_each(|t| print!("{:?} ", t));
    println!();

    let program = parse::parse(&program);
    println!("{:?}", program);

    let s = parse::parse(&token::tokenize("1"));
    assert_eq!(format!("{:?}", s), "1");

    let s = parse::parse(&token::tokenize("1 + 2 * 3"));
    assert_eq!(format!("{:?}", s), "(+ 1 (* 2 3))");

    let s = parse::parse(&token::tokenize("a + b * c * d + e"));
    assert_eq!(format!("{:?}", s), "(+ (+ a (* (* b c) d)) e)");

    let s = parse::parse(&token::tokenize("f . g . h"));
    assert_eq!(format!("{:?}", s), "(. f (. g h))");

    let s = parse::parse(&token::tokenize(" 1 + 2 + f . g . h * 3 * 4"));
    assert_eq!(format!("{:?}", s), "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))",);

    let s = parse::parse(&token::tokenize("- -1 * 2"));
    assert_eq!(format!("{:?}", s), "(* (- (- 1)) 2)");

    let s = parse::parse(&token::tokenize("- -f . g"));
    assert_eq!(format!("{:?}", s), "(- (- (. f g)))");

    let s = parse::parse(&token::tokenize("-9!"));
    assert_eq!(format!("{:?}", s), "(- (! 9))");

    let s = parse::parse(&token::tokenize("f . g !"));
    assert_eq!(format!("{:?}", s), "(! (. f g))");

    let s = parse::parse(&token::tokenize("(((0)))"));
    assert_eq!(format!("{:?}", s), "(( (( (( 0)))");

    let s = parse::parse(&token::tokenize("x[0][1]"));
    assert_eq!(format!("{:?}", s), "([ ([ x 0) 1)");

    let s = parse::parse(&token::tokenize(
        "a ? b :
         c ? d
         : e",
    ));
    assert_eq!(format!("{:?}", s), "(? a b (? c d e))");

    let s = parse::parse(&token::tokenize("a = 0 ? b : c = d"));
    assert_eq!(format!("{:?}", s), "(= a (= (? 0 b c) d))");
}
