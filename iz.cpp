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

	void reverse() {
		T tmp;
		for (size_t i = 0; i < len / 2; i++) {
			tmp = ptr[i];
			ptr[i] = ptr[len - 1 - i];
			ptr[len - 1 - i] = tmp;
		}
	}
};

template <typename T>
struct Buffer : public Slice<T> {
	using Slice<T>::len;
	size_t mem;
	using Slice<T>::ptr;

	static const size_t init = 16;

	Buffer() : Slice<T>(0, nullptr) {
		mem = init;
		ptr = (T*) malloc(mem * sizeof(T));
		assert(ptr != NULL, "malloc failed");
	}

	void resize(size_t size) {
		mem = size;
		ptr = (T*) realloc(ptr, mem * sizeof(T));
		assert(ptr != NULL, "realloc failed");
	}

	~Buffer() {
		free(ptr);
	}

	void push(T x) {
		if (len == mem) resize(mem * 2);
		ptr[len++] = x;
	}

	void extend(Slice<T> slice) {
		for (size_t i = 0; i < slice.len; i++) push(slice[i]);
	}
};

void print(Slice<char> slice) {
	for (size_t i = 0; i < slice.len; i++) printf("%c", slice[i]);
}

Buffer<char> escape(Slice<char> slice) {
	Buffer<char> out;
	for (size_t i = 0; i < slice.len; i++) {
		if (slice[i] == '\n') {
			out.push('\\');
			out.push('n');
		} else if (slice[i] == '\t') {
			out.push('\\');
			out.push('t');
		} else if (slice[i] == '"') {
			out.push('\\');
			out.push('"');
		} else {
			out.push(slice[i]);
		}
	}
	return out;
}

Buffer<char> to_string(size_t x) {
	Buffer<char> out;

	if (x == 0) {
		out.push('0');
	} else {
		for (size_t i = x; i > 0; i /= 10) out.push('0' + (i % 10));
		out.reverse();
	}

	return out;
}

/// compiler
struct Source {
	Slice<char> name;
	Slice<char> text;

	Source(Slice<char> name, Slice<char> text) : name(name), text(text) {}
};

struct Span {
	Source *source;
	Slice<char> slice;

	Span(Source *source, Slice<char> slice) : source(source), slice(slice) {}

	Buffer<char> error() {
		size_t row = 1, col = 1;
		for (char* ptr = source->text.ptr; ptr != slice.ptr; ptr++) {
			if (*ptr == '\n') {
				col = 1;
				row += 1;
			} else {
				col += 1;
			}
		}

		Buffer<char> out;

		out.push('"');
		out.extend(escape(slice));
		out.push('"');

		out.push(' ');
		out.push('a');
		out.push('t');
		out.push(' ');

		out.extend(source->name);
		out.push(':');
		out.extend(to_string(row));
		out.push(':');
		out.extend(to_string(col));

		return out;
	}
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
	Slice<char> name = {strlen(argv[1]), argv[1]};
	Source source = {name, text};

	Buffer<Span> tokens;
	for (size_t i = 0; i < text.len; i++) {
		Slice<char> slice = {1, &text[i]};
		Span token = {&source, slice};
		tokens.push(token);
	}

	// print tokens
	for (size_t i = 0; i < tokens.len; i++) {
		print(tokens[i].error());
		printf("\n");
	}
	printf("\n");

	return 0;
}
