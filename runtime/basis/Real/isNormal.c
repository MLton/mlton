#include <math.h>
#if (defined __sparc__)
#include <ieeefp.h>
#endif
#include "mlton-basis.h"
#include "my-lib.h"

#if (defined (__i386__))

#define EXPONENT_MASK 0x7FF00000

Int Real64_isNormal (Real64 d) {
	Word word1, exponent;

	word1 = ((Word *)&d)[1];
  
  	exponent = word1 & EXPONENT_MASK;

	return not (exponent == 0 or exponent == EXPONENT_MASK);
}

#elif (defined __sparc__)

Int Real64_isNormal (Real64 d) {
	fpclass_t c;

	c = fpclass (d);
	return c == FP_NNORM || c == FP_PNORM || c == FP_NZERO || c == FP_PZERO;
}

#else

#error Real64_isNormal not defined

#endif
