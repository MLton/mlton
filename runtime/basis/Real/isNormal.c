#include <math.h>
#include "mlton-basis.h"
#include "my-lib.h"

#if (defined (__i386__))

#define EXPONENT_MASK 0x7FF00000

Int Real_isNormal (Double d) {
	Word word1, exponent;

	word1 = ((Word *)&d)[1];
  
  	exponent = word1 & EXPONENT_MASK;

	return not (exponent == 0 or exponent == EXPONENT_MASK);
}

#elif (defined __sparc__)

Int Real_isNormal (Double d) {
	fpclass_t c;

	c = fpclass (d);
	return c == FP_NNORM || c == FP_PNORM || c == FP_NZERO || c == FP_PZERO;
}

#else

#error Real_isNormal not defined

#endif
