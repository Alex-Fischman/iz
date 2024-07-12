#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/// basic
#define assert(pred, msg) (void)((pred) || (__assert(msg, __FILE__, __LINE__), 0))
void __assert (const char *msg, const char *file, int line) {
	printf("%s at %s:%d\n", msg, file, line);
	exit(1);
}

template <typename T>
struct Buffer {
	size_t allocated, len;
	T* data;

	Buffer() {
		allocated = 16;
		len = 0;
		data = (T*) malloc(allocated * sizeof(T));
		assert(data != NULL, "malloc failed");
	}

	~Buffer() {
		free(data);
	}

	void resize(size_t size) {
		allocated = size;
		data = (T*) realloc(data, allocated * sizeof(T));
		assert(data != NULL, "realloc failed");
	}

	void push(T x) {
		if (len == allocated) resize(allocated * 2);
		data[len] = x;
		len++;
	}
};

struct String : Buffer<char> {
	void print() const {
		for (size_t i = 0; i < len; i++) printf("%c", data[i]);
	}
};

/// compiler
struct Source {
	const char *name;
	const char *text;
};

struct Token {
	const Source *src;
	size_t idx, len;

	String to_string() {
		String out;
		for (size_t i = idx; i < idx + len; i++) out.push(src->text[i]);
		return out;
	}
};

/// main
int main(int argc, char const *argv[]) {
	// check arguments
	if (argc != 2) {
		printf("usage: ./iz [file.iz]\n");
		return 1;
	}

	// open file
	FILE* file = fopen(argv[1], "r");
	if (file == NULL) {
		printf("could not open file");
		return 1;
	}

	// read file into memory
	String text;
	char c;
	while (fread(&c, 1, 1, file)) text.push(c);

	// close file
	fclose(file);

	// parse text into tokens
	Source src;
	src.name = argv[1];
	src.text = text.data;

	Buffer<Token> tokens;
	for (size_t i = 0; i < text.len; i++) {
		Token token;
		token.idx = i;
		token.len = 1;
		token.src = &src;
		tokens.push(token);
	}

	// print tokens
	for (size_t i = 0; i < tokens.len; i++) tokens.data[i].to_string().print();
	printf("\n");

	return 0;
}
