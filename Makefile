all: build test

build:
	@ zig fmt iz.zig
	@ zig build-exe iz.zig

test:
	@ ./iz scratch.iz

clean:
	rm iz
