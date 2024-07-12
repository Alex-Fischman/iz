all: build

build:
	$(CXX) -nostdinc++ -Wall -Wextra iz.cpp -o iz

clean:
	rm iz
