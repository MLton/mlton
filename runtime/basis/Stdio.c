#define _ISOC99_SOURCE

#include "platform.h"

void Stdio_print (Pointer s) {
	if (0 == Array_numElements (s))
		return;
	while (1 != fwrite (s, Array_numElements(s), 1, stderr))
		/* nothing */;
}

Int Stdio_sprintf (Pointer buf, Pointer fmt, Real x) {
	return sprintf (buf, (char*) fmt, x);
}
