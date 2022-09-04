enum Error {
	MissingCommandLineArgument,
	CouldNotReadFile(String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingCommandLineArgument => writeln!(f, "no command line argument"),
            Self::CouldNotReadFile(file) => writeln!(f, "could not read {}", file),
        }
    }
}

fn main() -> Result<(), Error> {
	let args: Vec<String> = std::env::args().collect();
	let file = args.get(1).ok_or(Error::MissingCommandLineArgument)?;
	let text = std::fs::read_to_string(file)
		.map_err(|_| Error::CouldNotReadFile(file.to_owned()))?;

	println!("{}", text);

	Ok(())
}
