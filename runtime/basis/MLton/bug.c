#include <stdio.h>
#include <stdlib.h>
#include "mlton-basis.h"

void MLton_bug (Pointer msg) {
	fprintf (stderr, "MLton bug: %s.\n%s\n",
			(char*)msg,
			"Please send a bug report to MLton@sourcelight.com.");
 	exit (2);
}
