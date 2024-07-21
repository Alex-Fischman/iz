all: build test

build:
	@ $(CXX) -nostdinc++ -Wall -Wextra iz.cpp -o iz

test:
	@ valgrind -q ./iz scratch.iz

clean:
	rm iz
