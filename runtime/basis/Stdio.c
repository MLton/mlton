#include <stdio.h>
#include "mlton-basis.h"
#include "my-lib.h"

void Stdio_print(Pointer s) {
/*		swrite(s, 1, Array_numElements(s), stderr); */
	while (1 != fwrite(s, Array_numElements(s), 1, stderr))
		/* nothing */;
}

Int Stdio_sprintf(Pointer buf, Pointer fmt, Double x) {
	return sprintf(buf, (char*) fmt, x);
}
