#include <math.h>
#if (defined __sparc__)
#include <ieeefp.h>
#endif
#include "mlton-basis.h"
#include "my-lib.h"

enum {
	DEBUG = FALSE,
};

/* All this code assumes IEEE 754/854 and little endian.
 *
 * In memory, the 64 bits of a double are layed out as follows.
 *
 * d[0]  bits 7-0 of mantissa
 * d[1]  bits 15-8 of mantissa
 * d[2]  bits 23-16 of mantissa
 * d[3]  bits 31-24 of mantissa
 * d[4]  bits 39-32 of mantissa
 * d[5]  bits 47-40 of mantissa
 * d[6]  bits 3-0 of exponent
 *       bits 51-48 of mantissa
 * d[7]  sign bit
 *       bits 10-4 of exponent
 */

#define Real_Class_nanQuiet 0
#define Real_Class_nanSignalling 1
#define Real_Class_inf 2
#define Real_Class_zero 3
#define Real_Class_normal 4
#define Real_Class_subnormal 5

#if (defined __i386__)

/* masks for word 1 */
#define EXPONENT_MASK 0x7FF00000
#define MANTISSA_MASK 0x000FFFFF
#define SIGNBIT_MASK  0x80000000
#define MANTISSA_HIGHBIT_MASK 0x00080000

Int Real64_class (Real64 d) {
	Word word0, word1;
	Int res;

	word0 = ((Word *)&d)[0];
	word1 = ((Word *)&d)[1];
	
	if ((word1 & EXPONENT_MASK) == EXPONENT_MASK) {
		/* NAN_QUIET, NAN_SIGNALLING, or INF */
		if (word0 or (word1 & MANTISSA_MASK)) {
			/* NAN_QUIET or NAN_SIGNALLING -- look at the highest bit of mantissa */
			if (word1 & MANTISSA_HIGHBIT_MASK)
				res = Real_Class_nanQuiet;
			else
				res = Real_Class_nanSignalling;
		} else
			res = Real_Class_inf;
	} else {
		/* ZERO, NORMAL, or SUBNORMAL */
		if (word1 & EXPONENT_MASK)
       			res = Real_Class_normal;
		else if (word0 or (word1 & MANTISSA_MASK))
			res = Real_Class_subnormal;
		else
			res = Real_Class_zero;
	}
	if (DEBUG)
		fprintf (stderr, "%d = Real_class (%g)\n", res, d);
	return res;
}

#elif (defined __sparc__)

Int Real64_class (Real64 d) {
	fpclass_t c;

	c = fpclass (d);
	switch (c) {
	case FP_SNAN: return Real_Class_nanSignalling;
	case FP_QNAN: return Real_Class_nanQuiet;
	case FP_NINF: return Real_Class_inf;
	case FP_PINF: return Real_Class_inf;
	case FP_NDENORM: return Real_Class_subnormal;
	case FP_PDENORM: return Real_Class_subnormal;
	case FP_NZERO: return Real_Class_zero;
	case FP_PZERO: return Real_Class_zero;
	case FP_NNORM: return Real_Class_normal;
	case FP_PNORM: return Real_Class_normal;
	default:
		die ("Real_class error: invalid class %d\n", c);
	}
}

#else

#error Real_class not defined

#endif
