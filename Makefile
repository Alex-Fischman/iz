all: build

build:
	$(CXX) -nostdinc++ iz.cpp -o iz

clean:
	rm iz
