#include <math.h>
#if (defined __sparc__)
#include <ieeefp.h>
#endif
#include "mlton-basis.h"

#if (defined (__i386__))

Int Real_isNan (Double d) {
	return isnan (d); /* isnan is from math.h */
}

#elif (defined __sparc__)

Int Real_isNan (Double d) {
	fpclass_t c;

	c = fpclass (d);
 	return c == FP_SNAN || c == FP_QNAN;
}

#else

#error Real_isNan not defined

#endif
