#include "platform.h"
#include "gdtoa/gdtoa.h"

Real32 Real32_strto (char *s) {
	char *endptr;
	Real32 res;

	res = strtof (s, &endptr);
	assert (NULL != endptr);
	return res;
}

Real64 Real64_strto (char *s) {
	char *endptr;
	Real64 res;

	res = strtod (s, &endptr);
	assert (NULL != endptr);
	return res;
}
