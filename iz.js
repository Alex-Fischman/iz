"use strict";
let lib = require("./lib");

const iz = process.argv[2];
const ll = process.argv[3];

function tokenize(string, affixes, matches) {
	let escaped = false;
	return Array.from(string)
		.map((c, i) => new lib.Token(c, i + 1))
		// Line comments
		.split((t, i, ts) => t.str == "~" && i + 1 < ts.length && ts[i + 1].str == "~")
		.map((ts, i) => i == 0? ts: ts.slice(ts.findIndex(t => t.str == "\n")))
		.flat()
		// Block comments
		.split(t => t.str == "~")
		.filter((_, i) => i % 2 == 0)
		.flat()
		// Strings
		.reduce((acc, t) => {
			if (acc.last() && acc.last().str.startsWith("\"")) {
				if (escaped) {
					escaped = false;
					if (t.str == "n") acc.last().str += "\n";
					else if (t.str == "t") acc.last().str += "\t";
					else if (t.str == "r") acc.last().str += "\r";
					else if (t.str == "v") acc.last().str += "\v";
					else if (t.str == "\"") acc.last().str += "\"";
					else if (t.str == "\\") acc.last().str += "\\";
					else acc.last().str += t.str;
				}
				else if (t.str == "\\") escaped = true;
				else {
					acc.last().str += t.str;
					if (t.str == "\"") acc.push(new lib.Token("", 0));
				}
			}
			else acc.push(t);
			return acc;
		}, [])
		// Commands
		.split((t, i, ts) => t.str == "#" && (i == 0 || (i > 1 && ts[i - 1].str == "\n")))
		.map((ts, i) => {
			if (i == 0) return ts;
			let j = ts.findIndex(t => t.str == "\n");
			let args = ts.slice(0, j)
				.split(t => t.str.isWhitespace())
				.filter(ts => ts.length != 0)
				.map(ts => ts.map(t => t.str).join(""));
			if (args.length == 7 && args[0] == "affix") affixes.push({
				name: args[1], func: args[2], arity: Number(args[3]),
				pos: Number(args[4]), prec: Number(args[5]), assoc: args[6]
			});
			else if (args.length == 4 && args[0] == "match") matches.push({
				opener: args[1], closer: args[2], func: args[3]
			});
			return ts.slice(j);
		})
		.flat()
		// Whitespace separation
		.split(t => t.str.isWhitespace())
		.filter(ts => ts.length != 0)
		.map(ts => new lib.Token(ts.map(t => t.str).join(""), ts[0].pos))
		// Operator separation
		.flatMap(t => affixes.map(a => a.name)
			.concat(matches.flatMap(m => [m.opener, m.closer]))
			.sort((a, b) => b.length - a.length)
			.reduce((ts, name, _, names) => ts.flatMap(t => {
				if (t.str.startsWith("\"") || 
					name.isAlphabetic() || 
					names.includes(t.str)) return [t];
				else {
					let ts = [];
					let i = 0;
					let j;
					while ((j = t.str.indexOf(name, i)) != -1) {
						ts.push(new lib.Token(t.str.slice(i, j), t.pos + i));
						ts.push(new lib.Token(name, t.pos + j));
						i = j + name.length;
					}
					if (i < t.str.length) {
						ts.push(new lib.Token(t.str.slice(i), t.pos + i));
					}
					return ts.filter(t => t.str.length != 0);
				}
			}), [t])
		);
}

function parse(tokens, affixes, matches) {
	let ast = new lib.Tree(new lib.Token("ROOT", 0));
	let stack = [{closer: "", tree: ast}];
	for (let t of tokens) {
		let match = matches.find(m => t.str == m.opener);
		if (match) {
			stack.last().tree.cs.push(new lib.Tree(t));
			stack.push({closer: match.closer, tree: stack.last().tree.cs.last()});
		}
		else if (t.str == stack.last().closer) stack.pop();
		else stack.last().tree.cs.push(new lib.Tree(t));
	}
	ast = ast
		.map(ast => {
			let opsInAST = [];
			for (let i = 0; i < ast.cs.length; i++) {
				for (let j = 0; j < affixes.length; j++) {
					if (ast.cs[i].v.str == affixes[j].name) {
						opsInAST.push({index: i, affix: affixes[j]});
					}
				}
			}
			opsInAST.sort((a, b) => {
				let precs = b.affix.prec - a.affix.prec;
				if (precs) return precs;
				return -1 * (a.affix.assoc == "l");
			});
			let out = ast;
			for (let i = 0; i < opsInAST.length; i++) {
				let start = opsInAST[i].index - opsInAST[i].affix.pos;
				let end = start + opsInAST[i].affix.arity;
				for (let j = start; j < end; j++) {
					out.cs[opsInAST[i].index].cs.push(out.cs[j]);
					out.cs.splice(j, 1);
					for (let k = 0; k < opsInAST.length; k++) {
						if (opsInAST[k].index > j) {
							opsInAST[k].index--;
						}
					}
				}
			}
			return out;
		})
		.map(ast => {
			if (ast.v.str == "ROOT") return ast;
			if (ast.v.str == "$") return new lib.Tree(new lib.Token("call", 0), ast.cs);

			let func = "";
			for (let a of affixes) if (a.name == ast.v.str) func = a.func;
			for (let m of matches) if (m.opener == ast.v.str) func = m.func;
			if (func == "") return ast;

			let out = new lib.Tree(new lib.Token("call", 0));
			out.cs.push(new lib.Tree(new lib.Token(func)));
			for (let c of ast.cs) out.cs.push(c);
			return out;
		});
	return ast;
}

const fs = require("fs");
fs.readFile(iz, "utf8", (err, data) => {
	if (err) console.error(err);
	else {
		let affixes = [];
		let matches = [];
		let tokens = tokenize(data, affixes, matches);
		let ast = parse(tokens, affixes, matches);
		console.log("" + ast);
	}
});
