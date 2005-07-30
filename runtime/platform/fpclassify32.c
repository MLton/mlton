/* masks for word 0 */
#define EXPONENT_MASK32 0x7F800000
#define MANTISSA_MASK32 0x007FFFFF
#define SIGNBIT_MASK32  0x80000000
#define MANTISSA_HIGHBIT_MASK32 0x00400000

int fpclassify32 (float f) {
	uint word0;
	int res;

	word0 = ((uint *)&f)[0];  /* this generates a gcc warning */
	if ((word0 & EXPONENT_MASK32) == EXPONENT_MASK32) {
		if (word0 & MANTISSA_MASK32)
			res = FP_NAN;
		else
			res = FP_INFINITE;
	} else if (word0 & EXPONENT_MASK32)
 		res = FP_NORMAL;
	else if (word0 & MANTISSA_MASK32)
		res = FP_SUBNORMAL;
	else
		res = FP_ZERO;
	return res;
}
