"use strict";

Array.prototype.split = function split(f) {
	let out = [[]];
	for (let i = 0; i < this.length; ++i) {
		if (f(this[i], i, this)) out.push([]);
		else out[out.length - 1].push(this[i]);
	}
	return out;
};

Array.prototype.last = function last() {
	return this[this.length - 1];
};

String.prototype.isWhitespace = function isWhitespace() {
	return Array.from(this).every(c => " \n\t\r\v".includes(c));
};

String.prototype.isAlphabetic = function isAlphabetic() {
	return /^[a-zA-Z]*$/.test(this);
};

class Token {
	constructor(str, pos) {
		this.str = str;
		this.pos = pos;
	}

	toString() {
		return this.str;
	}
}

class Tree {
	constructor(value, children = []) {
		this.v = value;
		this.cs = children;
	}

	toString(depth = 0) {
		return "-".repeat(depth) + "|" + this.v + this.cs.map(c => "\n" + c.toString(depth + 1)).join("");
	}

	map(f) {
		for (let i = 0; i < this.cs.length; i++) this.cs[i] = this.cs[i].map(f);
		return f(this);
	}
}

exports.Tree = Tree;
exports.Token = Token;
