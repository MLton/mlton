#include "mlton-basis.h"

void PackReal_update(Pointer a, Int offset, Double r) {
	char *p = (char*)&r;
	char *s = a + offset;
	int i;

	for (i = 0; i < 8; ++i) {
		s[i] = p[i];
	}
}
