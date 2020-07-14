JS = iz.js

IZ = test.iz
ASM = $(IZ:.iz=.S)
OUT = $(IZ:.iz=.out)

all: $(OUT)
	./$<

$(OUT): $(ASM)
	gcc $< -o $@

$(ASM): $(JS) $(IZ)
	node $(JS) $(IZ) $(ASM)
