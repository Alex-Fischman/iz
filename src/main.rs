mod interpret;
mod parse;
mod tokenize;

fn main() -> Result<(), String> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or_else(|| "no file passed".to_owned())?;
	let chars: Vec<char> = std::fs::read_to_string(file)
		.map_err(|_| format!("could not read {}", file))?
		.chars()
		.collect();
	fn main(chars: &[char]) -> Result<(), Error> {
		let tokens = tokenize::tokenize(chars)?;
		let trees = parse::parse(&tokens)?;
		println!("\n{:#?}", interpret::interpret(&trees)?);
		Ok(())
	}
	match main(&chars) {
		Ok(()) => Ok(()),
		Err(Error { message, location }) => {
			if location.0 != 0 || location.1 != 0 {
				let fold = |(row, col), &c| match c {
					'\n' => (row + 1, 1),
					_ => (row, col + 1),
				};
				let (row0, col0) = chars.iter().take(location.0).fold((1, 1), fold);
				let (row1, col1) =
					chars.iter().skip(location.0).take(location.1).fold((row0, col0), fold);
				Err(format!("{}\n{}:{}-{}:{}", message, row0, col0, row1, col1))
			} else {
				Err(message)
			}
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Location(pub usize, pub usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
	message: String,
	location: Location,
}

impl Error {
	pub fn new(s: &str, location: Location) -> Error {
		Error { message: s.to_owned(), location }
	}
}
