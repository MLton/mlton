#include "mlton-basis.h"

Real32 PackReal32_subVec (Pointer v, Int offset) {
	Real32 r;
	char *p = (char*)&r;
	char *s = v + offset;
	int i;

	for (i = 0; i < 4; ++i)
		p[i] = s[i];
 	return r;
}

Real32 PackReal32_subVecRev (Pointer v, Int offset) {
	Real32 r;
	char *p = (char*)&r;
	char *s = v + offset;
	int i;

	for (i = 0; i < 4; ++i)
		p[i] = s[3 - i];
 	return r;
}

Real64 PackReal64_subVec (Pointer v, Int offset) {
	Real64 r;
	char *p = (char*)&r;
	char *s = v + offset;
	int i;

	for (i = 0; i < 8; ++i)
		p[i] = s[i];
 	return r;
}

Real64 PackReal64_subVecRev (Pointer v, Int offset) {
	Real64 r;
	char *p = (char*)&r;
	char *s = v + offset;
	int i;

	for (i = 0; i < 8; ++i)
		p[i] = s[7 - i];
 	return r;
}
