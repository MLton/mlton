#include <stdio.h>
#include <stdlib.h>
#include "mlton-basis.h"

void MLton_allocTooLarge () {
	fprintf (stderr, "Out of memory: attempt to allocate more than %d bytes.\n", 0x7FFFFFFF);
 	exit (2);
}
