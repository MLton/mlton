#include <math.h>
#if (defined __sparc__)
#include <ieeefp.h>
#endif
#include "mlton-basis.h"

#if (defined (__i386__))

Int Real32_isNan (Real32 f) {
	return isnan (f); /* isnan is from math.h */
}

Int Real64_isNan (Real64 d) {
	return isnan (d); /* isnan is from math.h */
}

#elif (defined __sparc__)

Int Real64_isNan (Real64 d) {
	fpclass_t c;

	c = fpclass (d);
 	return c == FP_SNAN || c == FP_QNAN;
}

#else

#error Real64_isNan not defined

#endif
