#if (defined __i386__)

/* This code assumes IEEE 754/854 and little endian.
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
 *
 *
 * In memory, the 32 bits of a float are layed out as follows.
 *
 * d[0]  bits 7-0 of mantissa
 * d[1]  bits 15-8 of mantissa
 * d[2]  bit  0 of exponent
 *       bits 22-16 of mantissa
 * d[7]  sign bit
 *       bits 7-2 of exponent
 */

/* masks for word 1 */
#define EXPONENT_MASK64 0x7FF00000
#define MANTISSA_MASK64 0x000FFFFF
#define SIGNBIT_MASK64  0x80000000
#define MANTISSA_HIGHBIT_MASK64 0x00080000

int fpclassify64 (double d) {
	Word word0, word1;
	Int res;

	word0 = ((Word *)&d)[0];
	word1 = ((Word *)&d)[1];
	if ((word1 & EXPONENT_MASK64) == EXPONENT_MASK64) {
		if (word0 or (word1 & MANTISSA_MASK64))
       			res = FP_NAN;
		else
			res = FP_INFINITE;
	} else if (word1 & EXPONENT_MASK64)
		res = FP_NORMAL;
	else if (word0 or (word1 & MANTISSA_MASK64))
		res = FP_SUBNORMAL;
	else
		res = FP_ZERO;
	return res;
}

#else

#error fpclassify64 not implemented

#endif
