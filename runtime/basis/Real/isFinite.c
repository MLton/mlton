#include <math.h>
#if (defined __sparc__)
#include <ieeefp.h>
#endif
#include "mlton-basis.h"

Int Real64_isFinite (Real64 d) {
	return finite (d); /* finite is from math.h */
}
