#include <math.h>
#include "basis-constants.h"
#include "mlton-basis.h"

Double Real_Math_pi = M_PI;
Double Real_Math_e = M_E;
Double Real_posInf;

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

Int Real_signBit(Double d) {
	return (((unsigned char *)&d)[7] & 0x80) >> 7;
}

/* masks for word 1 */
#define EXPONENT_MASK 0x7FF00000
#define MANTISSA_MASK 0x000FFFFF
#define SIGNBIT_MASK  0x80000000
#define MANTISSA_HIGHBIT_MASK 0x00080000

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

/* returned by Real_class */
#define Real_Class_nanQuiet 0
#define Real_Class_nanSignalling 1
#define Real_Class_inf 2
#define Real_Class_zero 3
#define Real_Class_normal 4
#define Real_Class_subnormal 5

Int Real_class(Double d) {
	Word word0, word1;

	word0 = ((Word *)&d)[0];
	word1 = ((Word *)&d)[1];
	
	if ((word1 & EXPONENT_MASK) == EXPONENT_MASK) {
		/* NAN_QUIET, NAN_SIGNALLING, or INF */
		if (word0 or (word1 & MANTISSA_MASK)) {
			/* NAN_QUIET or NAN_SIGNALLING -- look at the highest bit of mantissa */
			if (word1 & MANTISSA_HIGHBIT_MASK)
				return Real_Class_nanQuiet;
			else
				return Real_Class_nanSignalling;
		} else
			return Real_Class_inf;
	} else {
		/* ZERO, NORMAL, or SUBNORMAL */
		if (word1 & EXPONENT_MASK)
       			return Real_Class_normal;
		else if (word0 or (word1 & MANTISSA_MASK))
			return Real_Class_subnormal;
		else
			return Real_Class_zero;
	}
}

Int Real_isFinite(Double d) {
	/* finite is from math.h */
	return finite(d);
}

Int Real_isNan(Double d) {
	Word word0, word1;

	word0 = ((Word *)&d)[0];
  	word1 = ((Word *)&d)[1];

	return (((word1 & EXPONENT_MASK) == EXPONENT_MASK)
		and (word0 or (word1 & MANTISSA_MASK)));
}

Int Real_qequal(Double x1, Double x2) {
	return Real_isNan(x1) || Real_isNan(x2) || x1 == x2;
}

Int Real_isNormal(Double d) {
	Word word1, exponent;

	word1 = ((Word *)&d)[1];
  
  	exponent = word1 & EXPONENT_MASK;

	return not(exponent == 0 or exponent == EXPONENT_MASK);
}

Double Real_round(Double d) {
	register double f0;

	f0 = d;
	__asm__ __volatile__ ("frndint"
			: "=t" (f0)
			: "0" (f0));
	return f0;

}
