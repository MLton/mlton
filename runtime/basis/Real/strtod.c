#include <stdio.h>
#include <gc.h>
#include "gdtoa.h"
#include "mlton-basis.h"
#include "my-lib.h"

Real64 Real64_strtod (char *s) {
	char *endptr;
	Real64 res;

	res = strtod (s, &endptr);
	assert (NULL != endptr);
	return res;
}
