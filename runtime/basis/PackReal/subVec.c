#include "mlton-basis.h"

Double PackReal_subVec(Pointer v, Int offset) {
	double r;
	char *p = (char*)&r;
	char *s = v + offset;
	int i;

	for (i = 0; i < 8; ++i)
		p[i] = s[i];
 	return r;
}
