all: build test

build:
	@ zig fmt iz.zig
	@ zig build-exe iz.zig

test:
	@ valgrind -q ./iz scratch.iz

clean:
	rm iz
