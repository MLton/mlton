#include <stdio.h>
#include <gc.h>
#include "gdtoa.h"
#include "mlton-basis.h"
#include "my-lib.h"

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
