struct Token {
    source: std::rc::Rc<Source>,
    lo: usize,
    hi: usize,
}

struct Source {
    name: String,
    text: String,
}

impl std::ops::Deref for Token {
    type Target = str;
    fn deref(&self) -> &str {
        &self.source.text[self.lo..self.hi]
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let name = match args.get(1) {
        Some(name) => name.to_owned(),
        None => panic!("usage: pass a .iz file"),
    };
    let text = match std::fs::read_to_string(&name) {
        Ok(text) => text,
        Err(err) => panic!("could not read {}: {}", name, err),
    };
    let source = std::rc::Rc::new(Source { name, text });

    let los = source.text.char_indices().map(|(i, _)| i);
    let his = source.text.char_indices().map(|(i, _)| i);
    for (lo, hi) in los.zip(his.skip(1).chain([source.text.len()])) {
        let token = Token { source: source.clone(), lo, hi };
    }
}
