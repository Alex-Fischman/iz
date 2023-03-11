#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

template<typename T> class Opt {
	T data;
	bool is_some;

public:
	Opt none() {
		this.is_some = false;
	}

	Opt some(T data) {
		this.data = data;
		this.is_some = true;
	}

	T unwrap();
};

template<typename T> class Vec {
	T* data;
	int64_t len;
	int64_t cap;

	void reserve(int64_t new_cap) {
		if (this->cap >= new_cap) return;
		T* new_data = (T*) malloc(sizeof(T) * new_cap);
		for (int64_t i = 0; i < this->cap; i++) new_data[i] = this->data[i];
		free(this->data);
		this->data = new_data;
		this->cap = new_cap;
	}

public:
	Vec() {
		this->data = (T*) malloc(sizeof(T) * 16);
		this->len = 0;
		this->cap = 16;
	}

	~Vec() {
		free(this->data);
	}
	
	Opt<T> get(int64_t i) {
		if (i >= 0 && i < this->len) {
			return Opt<T>::some(this->data[i]);
		} else {
			return Opt<T>::none();
		}
	}

	void push(T x) {
		if (this->len == this->cap) this->reserve(this->cap * 2);
		this->data[this->len] = x;
		this->len++;
	}

	Opt<T> pop() {
		if (this->len > 0) {
			this->len--;
			return Opt<T>::some(this->data[this->len]);
		} else {
			return Opt<T>::none();
		}
	}

	Opt<T> first();
	Opt<T> last();
};

int main(int argc, char const *argv[]) {
	Vec<int64_t> ints = Vec<int64_t>();
	for (int64_t i = 0; i < 1000000; i++) ints.push(i);

	printf("Hello, World!\n");
	return 0;
}
