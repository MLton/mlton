#include <stdio.h>
#include "platform.h"
#include "opcode.h"

int main () {
	int i;

	for (i = 0; i < cardof (opcodeStrings); ++i)
		fprintf (stdout, "%s\n", opcodeStrings[i]);
	return 0;
}
