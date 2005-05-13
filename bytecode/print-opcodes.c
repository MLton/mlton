#include "platform.h"
#include <stdio.h>
#include "opcode.h"

int main () {
	Opcode opc;
	int i;

	unless (cardof (opcodeStrings) < (1 << (8 * sizeof (opc))))
		die ("too many opcodes\n");
	for (i = 0; i < cardof (opcodeStrings); ++i)
		fprintf (stdout, "%s\n", opcodeStrings[i]);
	return 0;
}
