"use strict";

const ifile = process.argv[2], ofile = process.argv[3];
const opener = s => /^[([{]$/.test(s), closer = s => /^[)\]}]$/.test(s);
// TEMP
const logTree = (tree, depth = 0) => {
	console.log("-".repeat(depth) + "|" + tree.value);
	tree.children.forEach(c => logTree(c, depth + 1));
};

const fs = require("fs");
fs.readFile(ifile, "utf8", (err, data) => {
	if (err) console.log(err);
	else {
		const {tokens, ops} = data
			.match(/~~.*?\n|~.*?~|#.*?\n|"(\\"|.)*?"|[^\s(){}[\],;]+|[^\s,;]/gs)
			.reduce((acc, s) => {
				if (s[0] === "~") {}
				else if (s[0] === "#") {
					const args = s.split(" ");
					if (args[0] === "#operator") acc.ops[args[1]] = {
						prec: Number(args[2]), 
						arity: Number(args[3]), 
						left: args[4] === "l", 
						func: args[5].slice(0, -1)
					};
				}
				else acc.tokens.push(s.replace(/(?<!\\)\\n/g, "\n").replace(/\\(.)/gs, "$1"));
				return acc;
			}, {tokens: [], ops: {}});

		const ast = ["{", ...tokens, "}"].reduce((tree, token) => {
			if (opener(token)) {
				const last = tree.children[tree.children.length - 1];
				const node = {value: token, children: [], parent: tree};
				const parent = !last || ops.hasOwnProperty(last.value)? tree: last;
				parent.children.push(node);
				return node;
			}
			else if (closer(token)) {
				tree.children
					.filter(c => ops.hasOwnProperty(c.value))
					.sort((a, b) => ops[a.value].prec < ops[b.value].prec || ops[a.value].prec === ops[b.value].prec && ops[b.value].left? 1: -1)
					.forEach(op => {
						const open = {value: "(", children: [], parent: op};
						op.children.push(open);
						const moveToOpen = i => {
							open.children.unshift(tree.children[i]);
							tree.children.splice(i, 1);
							open.children[0].parent = open;
						};
						const i = tree.children.findIndex(c => c === op);
						if (ops[op.value].arity === 2 || !ops[op.value].left) moveToOpen(i + 1);
						if (ops[op.value].arity === 2 ||  ops[op.value].left) moveToOpen(i - 1);
						op.value = ops[op.value].func;
					});
				return tree.parent;
			}
			else {
				tree.children.push({value: token, children: [], parent: tree});
				return tree;
			}
		}, {value: "{", children: [], parent: null});
		
		let id = 0;
		const generate = (env, tree) => {
			let asm = "";
			if (!tree.children.length) {
				if (!isNaN(tree.value)) asm = `mov $${tree.value}, %rax\n`;
				else if (env.hasOwnProperty(tree.value)) asm = env[tree.value] === tree.value? `lea ${tree.value}(%rip), %rax\n`: `mov ${env[tree.value] * -8}(%rbp), %rax\n`;
				else console.log("Unrecognized: " + tree.value);
			}
			else {
				const args = tree.children[0].children;
				if (tree.value === "emit") asm = args[0].value.slice(1, -1);
				else if (opener(tree.value)) asm = tree.children.map(generate.bind(null, {...env})).map(e => e.asm).join("");
				else if (tree.value === "=") {
					asm = generate(env, args[1]).asm;
					if (env[args[0].value] !== args[0].value) {
						if (env.hasOwnProperty(args[0].value)) {
							asm += `mov %rax, ${env[args[0].value] * -8}(%rbp)\n`;
						}
						else {
							env[args[0].value] = 1 + Math.max(0, ...Object.values(env).filter(v => env[v] !== v));
							asm += "push %rax\n";
						}
					}
				}
				else if (tree.value === "->") {
					const named = tree.parent.parent.value === "=";
					let name = named? tree.parent.children[0].value: `f${id++}`;
					if (named) env[name] = name;

					const innerE = Object.fromEntries([...Object.entries(env).filter(([k, v]) => k === v), ...opener(args[0].value)? args[0].children.map(a => a.value).map((v, i, a) => [v, i - a.length - 1]): [[args[0].value, -2]]]);
					asm = `jmp jump_${name}\n${name}:\npush %rbp\nmov %rsp, %rbp\n${generate(innerE, args[1]).asm}mov %rbp, %rsp\npop %rbp\nret\njump_${name}:\nlea ${name}(%rip), %rax\n`;
				}
				else if (env.hasOwnProperty(tree.value)) asm = args.map(generate.bind(null, {...env})).map(e => e.asm + "push %rax\n").join("") + `call ${env[tree.value] === tree.value? tree.value: `*${env[tree.value] * -8}(%rbp)`}\nadd $${args.length * 8}, %rsp\n`;
				else console.log("Unrecognized: " + tree.value);
			}
			return {asm: asm, env: env};
		};

		fs.writeFile(ofile, generate({}, ast).asm, "utf8", err => {if (err) console.log(err);});
	}
});
