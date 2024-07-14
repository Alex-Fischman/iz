#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// basic
#define assert(pred, msg) (void)((pred) || (__assert(msg, __FILE__, __LINE__), 0))
void __assert (char const *msg, char const *file, int line) {
	printf("%s at %s:%d\n", msg, file, line);
	exit(1);
}

template <typename T>
struct Slice {
	size_t len;
	T *ptr;

	Slice(size_t len, T *ptr) : len(len), ptr(ptr) {}

	T& operator[](size_t i) {
		assert(i < len, "index out of bounds");
		return ptr[i];
	}
};

template <typename T>
struct Buffer {
	size_t len;
	Slice<T> slice;

	static const size_t init = 16;

	Buffer() : len(0), slice(0, NULL) {
		slice.len = init;
		slice.ptr = (T*) malloc(init * sizeof(T));
		assert(slice.ptr != NULL, "malloc failed");
	}

	void resize(size_t size) {
		slice.len = size;
		slice.ptr = (T*) realloc(slice.ptr, size * sizeof(T));
		assert(slice.ptr != NULL, "realloc failed");
	}

	~Buffer() {
		free(slice.ptr);
	}

	void push(T x) {
		if (len == slice.len) resize(len * 2);
		slice[len++] = x;
	}

	T& operator[](size_t i) {
		assert(i < len, "index out of bounds");
		return slice[i];
	}
};

void print(Slice<char> slice) {
	for (size_t i = 0; i < slice.len; i++) printf("%c", slice[i]);
}

/// compiler
struct Source {
	Slice<char> name;
	Slice<char> text;

	Source(Slice<char> name, Slice<char> text) : name(name), text(text) {}
};

struct Token {
	Source *source;
	Slice<char> slice;

	Token(Source *source, Slice<char> slice) : source(source), slice(slice) {}
};

/// main
int main(int argc, char *argv[]) {
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
	Buffer<char> text;
	char c;
	while (fread(&c, 1, 1, file)) text.push(c);

	// close file
	fclose(file);

	// parse text into tokens
	Slice<char> name(strlen(argv[1]), argv[1]);
	Source source(name, text.slice);

	Buffer<Token> tokens;
	for (size_t i = 0; i < text.len; i++) {
		Slice<char> slice(1, &text[i]);
		Token token(&source, slice);
		tokens.push(token);
	}

	// print tokens
	for (size_t i = 0; i < tokens.len; i++) print(tokens[i].slice);
	printf("\n");

	return 0;
}
