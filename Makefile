CPP = iz.cpp
IZ = test.iz
LL = test.ll

all:
	g++ $(CPP) -o iz -Wall -std=c++2a
	./iz $(IZ) $(LL)
