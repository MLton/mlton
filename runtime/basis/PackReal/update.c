#include "mlton-basis.h"

void PackReal32_update (Pointer a, Int offset, Real32 r) {
	char *p = (char*)&r;
	char *s = a + offset;
	int i;

	for (i = 0; i < 4; ++i) {
		s[i] = p[i];
	}
}

void PackReal32_updateRev (Pointer a, Int offset, Real32 r) {
	char *p = (char*)&r;
	char *s = a + offset;
	int i;

	for (i = 0; i < 4; ++i) {
		s[i] = p[3 - i];
	}
}

void PackReal64_update (Pointer a, Int offset, Real64 r) {
	char *p = (char*)&r;
	char *s = a + offset;
	int i;

	for (i = 0; i < 8; ++i) {
		s[i] = p[i];
	}
}

void PackReal64_updateRev (Pointer a, Int offset, Real64 r) {
	char *p = (char*)&r;
	char *s = a + offset;
	int i;

	for (i = 0; i < 8; ++i) {
		s[i] = p[7 - i];
	}
}
