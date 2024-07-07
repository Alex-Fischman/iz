#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// sane library functions
#define assert(pred, msg) (void)((pred) || (__assert(msg, __FILE__, __LINE__), 0))
void __assert (const char *msg, const char *file, int line) {
	printf("%s at %s:%s\n", msg, file, line);
	exit(1);
}

// library data structures
template <typename T>
struct buffer {
	size_t len;
	T* data;

	buffer(size_t init) {
		len = init;
		data = (T*) malloc(init * sizeof(T));
		assert(data != NULL, "malloc failed");
	}

	~buffer() {
		free(data);
	}

	void resize(size_t init) {
		len = init;
		data = (T*) realloc(data, init * sizeof(T));
		assert(data != NULL, "realloc failed");
	}
};

template <typename T>
struct vector {
	size_t len;
	buffer<T> buf;

	vector()            : len(0), buf(16)   {}
	vector(size_t init) : len(0), buf(init) {}

	T  operator[](size_t i) const { return buf.data[i]; }
	T& operator[](size_t i)       { return buf.data[i]; }

	void push(T x) {
		if (len == buf.len) buf.resize(buf.len * 2);
		buf.data[len] = x;
		len++;
	}

	template <typename S>
	vector<S> map(S func(const T&)) {
		vector<S> out(buf.len);
		out.len = len;
		for (size_t i = 0; i < buf.len; i++) out.buf.data[i] = func(buf.data[i]);
		return out;
	}
};

// compiler data structures
struct token {
	char const *start;
	size_t len;

	static token from_char(const char &c) {
		token out;
		out.start = &c;
		out.len = 1;
		return out;
	}
};

// main function
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
	vector<char> text;
	char c;
	while (fread(&c, 1, 1, file)) text.push(c);

	// close file
	fclose(file);

	// parse text into tokens
	vector<token> tokens = text.map(token::from_char);

	// print tokens
	for (size_t i = 0; i < tokens.len; i++) printf("%c", *tokens[i].start);
	printf("\n");

	return 0;
}
