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

	vector() : len(0), buf(16) {}

	T  operator[](int i) const { return buf.data[i]; }
	T& operator[](int i)       { return buf.data[i]; }

	void push(T x) {
		if (len == buf.len) buf.resize(buf.len * 2);
		buf.data[len] = x;
		len++;
	}
};

// compiler data structures
struct Token {
	char* start;
	size_t len;
};

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

	// print text
	for (size_t i = 0; i < text.len; i++) printf("%c", text[i]);
	printf("\n");

	return 0;
}
