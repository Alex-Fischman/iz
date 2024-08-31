#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/// basic
#define panic(message) __panic(message, __FILE__, __LINE__)
void __panic(char const *message, char const *file, int line) {
	printf("%s at %s:%d\n", message, file, line);
	exit(1);
}

template <typename T>
struct Slice {
	size_t len;
	T *ptr;

	Slice(size_t len, T *ptr) : len(len), ptr(ptr) {}

	T& operator[](size_t i) {
		if (i >= len) panic("index out of bounds");
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

	// can't be defined inline because C++
	template <typename S> friend Buffer<S> alloc();

	void resize(size_t size) {
		mem = size;
		ptr = (T*) realloc(ptr, mem * sizeof(T));
		if (ptr == nullptr) panic("realloc failed");
	}

	friend void free(Buffer<T>& buf) {
		::free(buf.ptr);
		buf.ptr = nullptr;
	}

	~Buffer() {
		if (ptr != nullptr) panic("memory leak");
	}

	void push(T x) {
		if (len == mem) resize(mem * 2);
		ptr[len++] = x;
	}

	void extend(Slice<T> slice) {
		for (size_t i = 0; i < slice.len; i++) push(slice[i]);
	}

private:
	Buffer(size_t len, T *ptr) : Slice<T>(len, ptr) {}
};

template <typename T>
Buffer<T> alloc() {
	Buffer<T> out = {0, nullptr};
	out.mem = Buffer<T>::init;
	out.ptr = (T*) malloc(out.mem * sizeof(T));
	if (out.ptr == nullptr) panic("malloc failed");
	return out;
}

void print(char c) {
	printf("%c", c);
}

void print(Slice<char> slice) {
	for (size_t i = 0; i < slice.len; i++) print(slice[i]);
}

Buffer<char> escape(Slice<char> slice) {
	auto out = alloc<char>();
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
	auto out = alloc<char>();

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
		for (char *ptr = source->text.ptr; ptr != slice.ptr; ptr++) {
			if (*ptr == '\n') {
				col = 1;
				row += 1;
			} else {
				col += 1;
			}
		}

		auto out = alloc<char>();

		auto escaped = escape(slice);
		auto row_string = to_string(row);
		auto col_string = to_string(col);

		out.push('"');
		out.extend(escaped);
		out.push('"');

		out.push(' ');
		out.push('a');
		out.push('t');
		out.push(' ');

		out.extend(source->name);
		out.push(':');
		out.extend(row_string);
		out.push(':');
		out.extend(col_string);

		free(escaped);
		free(row_string);
		free(col_string);

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
	if (file == nullptr) {
		printf("could not open file");
		return 1;
	}

	// read file into memory
	auto text = alloc<char>();
	char c;
	while (fread(&c, 1, 1, file)) text.push(c);

	// close file
	fclose(file);

	// parse text into spans
	Slice<char> name = {strlen(argv[1]), argv[1]};
	Source source = {name, text};

	auto spans = alloc<Span>();
	for (size_t i = 0; i < text.len; i++) {
		Slice<char> slice = {1, &text[i]};
		Span span = {&source, slice};
		spans.push(span);
	}

	// print spans
	for (size_t i = 0; i < spans.len; i++) {
		auto message = spans[i].error();
		print(message);
		print('\n');
		free(message);
	}
	printf("\n");

	free(spans);
	free(text);

	return 0;
}
