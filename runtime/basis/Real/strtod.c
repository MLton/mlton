#include <stdio.h>
#include <gc.h>
#include "gdtoa.h"
#include "mlton-basis.h"
#include "my-lib.h"

Double Real_strtod (char *s) {
	char *endptr;
	Double res;

	res = strtod (s, &endptr);
	assert (NULL != endptr);
	return res;
}
