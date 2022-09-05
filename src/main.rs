enum Error {
    MissingCommandLineArgument,
    CouldNotReadFile(String),
    MissingEndQuote(Location),
	MissingEscapeCharacter(Location),
    InvalidEscapeCharacter(Location),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingCommandLineArgument => writeln!(f, "no command line argument"),
            Error::CouldNotReadFile(file) => writeln!(f, "could not read {}", file),
            _ => todo!(),
        }
    }
}

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().collect();
    let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
    let text =
        std::fs::read_to_string(file).map_err(|_| Error::CouldNotReadFile(file.to_owned()))?;
    let chars: Vec<char> = text.chars().collect();

    let tokens = tokenize(&chars)?;
    for token in tokens {
        println!("{}", token.to_string(&file, &chars));
    }

    Ok(())
}

#[derive(Clone, Copy)]
struct Location {
    idx: usize,
    len: usize,
}

impl Location {
	fn new(idx: usize, len: usize) -> Location {
		Location { idx, len }
	}
	
	fn to_string(&self, chars: &[char]) -> String {
		let mut row = 1;
        let mut col = 1;
        for char in chars {
            if *char == '\n' {
                row += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
		format!("@{}:{}", row, col)
	}
}

#[derive(Clone, Copy)]
enum Bracket {
    Round,
    Curly,
    Square,
}

#[derive(Clone, Copy)]
enum TokenData {
    Identifier,
    String,
    Number(i64),
    OpenBracket(Bracket),
    CloseBracket(Bracket),
}

struct Token(Location, TokenData);

impl Token {
    fn to_string(&self, file: &str, chars: &[char]) -> String {
        chars[self.0.idx..self.0.idx + self.0.len]
            .iter()
            .collect::<String>()
            + &self.0.to_string(chars)
    }
}

fn tokenize(chars: &[char]) -> Result<Vec<Token>, Error> {
    let mut out = vec![];
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '"' => {
				i += 1;
                let mut j = 0;
                loop {
					if i + j >= chars.len() {
						Err(Error::MissingEndQuote(Location::new(i + j, 0)))?
					}
                    match chars[i + j] {
                        '"' => break,
                        '\\' => {
                            j += 1;
							if i + j >= chars.len() {
								Err(Error::MissingEscapeCharacter(Location::new(i + j, 0)))?
							}
                            match chars[i + j] {
                                '\\' | '"' | 'n' | 't' => {}
                                _ => Err(Error::InvalidEscapeCharacter(Location::new(i + j, 1)))?
                            }
                        }
                        _ => {}
                    }
                    j += 1;
                }
				out.push(Token(Location::new(i, j - 1), TokenData::String));
                i += j;
            }
			'#' => {
				i += 1;
				while i < chars.len() && chars[i] != '\n' {
					i += 1;
				}
			},
			'(' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Round))),
			'{' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Curly))),
			'[' => out.push(Token(Location::new(i, 1), TokenData::OpenBracket(Bracket::Square))),
			')' => out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Round))),
			'}' => out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Curly))),
			']' => out.push(Token(Location::new(i, 1), TokenData::CloseBracket(Bracket::Square))),
			' ' | '\t' | '\n' => {},
            _ => {
				let mut loc = Location::new(i, 1);
				while i + loc.len < chars.len() && match chars[i + loc.len] {
					'"' | '#' | '(' | '{' | '[' | ')' | '}' | ']' | ' ' | '\t' | '\n' => false,
					_ => true,
				} {
					loc.len += 1;
				}
				i += loc.len - 1;
				todo!("try to convert chars at loc to a number")
			},
        }
        i += 1;
    }
    Ok(out)
}
