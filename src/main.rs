mod parse;
mod tokenize;
mod analyze;

fn main() -> Result<(), String> {
	let args = std::env::args().collect::<Vec<String>>();
	let file = args.get(1).ok_or_else(|| "no file passed".to_owned())?;
	let chars: Vec<char> = std::fs::read_to_string(file)
		.map_err(|_| format!("could not read {}", file))?
		.chars()
		.collect();
	let tokens = tokenize::tokenize(&chars)?;
	let asts = parse::parse(&tokens)?;
	let tree = analyze::analyze(&asts)?;
	println!("\n{:#?}", tree);
	Ok(())
}
