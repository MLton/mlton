#include "platform.h"

#if HAS_FPCLASSIFY

C_Int_t Real32_class (Real32_t f) {
  return fpclassify (f);
}

#elif HAS_FPCLASSIFY32

C_Int_t Real32_class (Real32_t f) {
  return fpclassify32 (f);
}

#else

/* This code assumes IEEE 754/854 and little endian.
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

/* masks for word 0 */
#define EXPONENT_MASK32 0x7F800000
#define MANTISSA_MASK32 0x007FFFFF
#define SIGNBIT_MASK32  0x80000000
#define MANTISSA_HIGHBIT_MASK32 0x00400000

C_Int_t Real32_class (Real32_t f) {
  uint32_t word0;
  int res;

  word0 = ((uint32_t *)&f)[0];  /* this generates a gcc warning */
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

#endif


#if HAS_FPCLASSIFY

C_Int_t Real64_class (Real64_t d) {
  return fpclassify (d);
}

#elif HAS_FPCLASSIFY64

C_Int_t Real64_class (Real64_t d) {
  return fpclassify64 (d);
}

#else

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
 */

/* masks for word 1 */
#define EXPONENT_MASK64 0x7FF00000
#define MANTISSA_MASK64 0x000FFFFF
#define SIGNBIT_MASK64  0x80000000
#define MANTISSA_HIGHBIT_MASK64 0x00080000

C_Int_t Real64_class (Real64_t d) {
  uint32_t word0, word1;
  int res;

  word0 = ((uint32_t*)&d)[0];
  word1 = ((uint32_t*)&d)[1];
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

#error Real64_class not implemented

#endif

#endif
