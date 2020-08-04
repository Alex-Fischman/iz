"use strict";

// TODO: add stack closures by holding an environment pointer to the stack

const ifile = process.argv[2], ofile = process.argv[3];
const opener = s => /^[([{]/  .test(s);
const closer = s => /^[)\]}]/ .test(s);
const ignore = s => /^[\s,;~]/.test(s);
// TEMP
const logTree = (tree, depth = 0) => {
	console.log("-".repeat(depth) + "|" + tree.value);
	tree.children.forEach(c => logTree(c, depth + 1));
};

const fs = require("fs");
fs.readFile(ifile, "utf8", (err, data) => {
	if (err) {
		console.log(err);
		return;
	}

	const regex = /~~.*?\n|~.*?~|#.*?\n|"(\\"|.)*?"|[^\s(){}[\],;]+|./gs;
	const tokens = ["{", ...data.match(regex), "}"];

	let ast = {value: "{", children: [], parent: null};
	const opInfo = {};
	for (let i = 0; i < tokens.length; ++i) {
		if (tokens[i][0] === "#") {
			const args = tokens[i].split(" ");
			if (args[0] === "#operator") opInfo[args[1]] = {
				prec: Number(args[2]), 
				arity: Number(args[3]), 
				left: args[4] === "l", 
				func: args[5].slice(0, -1)
			};
		}
		else if (opener(tokens[i])) {
			const node = {value: tokens[i], children: [], parent: ast};
			const prev = tokens[i - 1];
			if (prev && !ignore(prev) && !opInfo.hasOwnProperty(prev)) {
				node.value = "call";
				node.children.push(ast.children.pop());
			}
			ast.children.push(node);
			ast = node;
		}
		else if (closer(tokens[i])) {
			const opList = ast.children.map((_, i) => i)
				.filter(i => opInfo.hasOwnProperty(ast.children[i].value))
				.sort((a, b) => {
					const infoA = opInfo[ast.children[a].value];
					const infoB = opInfo[ast.children[b].value];
					const tiebreaker = infoA.prec === infoB.prec && infoB.left;
					return infoB.prec > infoA.prec || tiebreaker? 1: -1;
				});
			for (let j = 0; j < opList.length; ++j) {
				const info = opInfo[ast.children[opList[j]].value];
				const call = {value: "call", children: [], parent: ast};
				const moveToCall = k => {
					call.children.push(ast.children[k]);
					ast.children[k].parent = call;
					ast.children.splice(k, 1);
					for (let l = 0; l < opList.length; ++l) 
						if (opList[l] > k) --opList[l];
				};
				call.children.push({value: info.func, children: []});
				if (info.arity === 2 ||  info.left) moveToCall(opList[j] - 1);
				if (info.arity === 2 || !info.left) moveToCall(opList[j] + 1);
				ast.children[opList[j]] = call;
			}
			ast = ast.parent;
		}
		else if (!ignore(tokens[i])) ast.children.push({
			value: tokens[i].replace(/(?<!\\)\\n/g, "\n").replace(/\\"/g, "\""),
			children: [], parent: ast
		});
	}

	let id = 0;
	const generate = (env, tree) => {
		let asm = "";
		if (!isNaN(tree.value)) 
			asm = `mov $${tree.value}, %rax\n`;
		else if (env.named.includes(tree.value)) 
			asm = `lea ${tree.value}(%rip), %rax\n`;
		else if (env.stack.hasOwnProperty(tree.value)) 
			asm = `mov ${env.stack[tree.value] * -8}(%rbp), %rax\n`;
		else if (opener(tree.value)) {
			const copied = {stack: {...env.stack}, named: [...env.named]};
			asm = tree.children.map(c => generate(copied, c).asm).join("");
		}
		else if (tree.value === "call") {
			const name = tree.children[0].value, args = tree.children.slice(1);
			if (name === "emit") asm = args[0].value.slice(1, -1);
			else if (name === "assign") {
				asm = generate(env, args[1]).asm;
				if (env.stack.hasOwnProperty(args[0].value)) 
					asm += `mov %rax, ${env.stack[args[0].value] * -8}(%rbp)\n`;
				else {
					const top = Math.max(0, ...Object.values(env.stack));
					env.stack[args[0].value] = top + 1;
					asm += "push %rax\n";
				}
			}
			else if (name === "arrow") {
				const siblings = tree.parent.children;
				const isNamed = siblings[0].value === "assign";
				const funcName = isNamed? siblings[1].value: `f${id++}`;
				if (isNamed) env.named.push(funcName);

				const first = args[0];
				const funcArgs = first.value === "("? first.children: [first];
				const innerStack = Object.fromEntries(funcArgs.map((t, i, a) => [t.value, i - a.length - 1])); // CLEAN 80
				const innerEnv = {named: env.named, stack: innerStack};

				asm  = `jmp jump_${funcName}\n${funcName}:\n`;
				asm += `push %rbp\nmov %rsp, %rbp\n`;
				asm += generate(innerEnv, args[1]).asm;
				asm += `mov %rbp, %rsp\npop %rbp\nret\n`;
				asm += `jump_${funcName}:\nlea ${funcName}(%rip), %rax\n`;
			}
			else {
				// CLEAN 80
				asm = args.map(a => generate(env, a).asm + "push %rax\n").join("");
				if (env.named.includes(name)) asm += `call ${name}\n`;
				else if (env.stack.hasOwnProperty(name)) asm += `call *${env.stack[name] * -8}(%rbp)\n`;
				else asm += generate(env, tree.children[0]).asm + "call *%rax\n";
				asm += `add $${args.length * 8}, %rsp\n`;
			}
		}
		else console.log("Unrecognized: " + tree.value);
		return {asm: asm, env: env};
	};

	const asm = generate({stack: {}, named: []}, ast).asm;
	fs.writeFile(ofile, asm, "utf8", err => {if (err) console.log(err);});
});
