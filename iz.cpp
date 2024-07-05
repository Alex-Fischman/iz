#include <stdint.h>
#include <stdio.h>

int main(int argc, char const *argv[]) {
	if (argc != 2) {
		printf("usage: ./iz [file.iz]\n");
		return 1;
	}

	FILE* file = fopen(argv[1], "r");

	char buffer;
	while (fread(&buffer, 1, 1, file)) printf("%c", buffer);

	fclose(file);

	printf("\n");

	return 0;
}
