#include <stdio.h>
#include "export.h"

void g () {
	Char c;

	fprintf (stderr, "g starting\n");
	c = f (13, 17.15);
	fprintf (stderr, "g done  char = %c\n", c);
}

Pointer g2 () {
	Pointer res;
	fprintf (stderr, "g2 starting\n");
	res = f2 (0xFF);
	fprintf (stderr, "g2 done\n");
	return res;
}

void g3 () {
	fprintf (stderr, "g3 starting\n");
	f3 ();
	fprintf (stderr, "g3 done\n");
}

void g4 (Int i) {
	fprintf (stderr, "g4 (%d)\n", i);
	f4 (i);
}
