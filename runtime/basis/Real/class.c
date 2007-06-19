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

/* This code assumes IEEE 754/854.
 *
 * In little-endian memory, the 32 bits of a float are layed out as follows.
 *
 * d[0]  bits 7-0 of mantissa
 * d[1]  bits 15-8 of mantissa
 * d[2]  bit  0 of exponent
 *       bits 22-16 of mantissa
 * d[3]  sign bit
 *       bits 7-1 of exponent
 *
 * In big-endian memory, the 32 bits of a float are layed out as follows.
 *
 * d[3]  bits 7-0 of mantissa
 * d[2]  bits 15-8 of mantissa
 * d[1]  bit  0 of exponent
 *       bits 22-16 of mantissa
 * d[0]  sign bit
 *       bits 7-1 of exponent
 */

/* masks for least/most significant word */
#define EXPONENT_MASK32 0x7F800000
#define MANTISSA_MASK32 0x007FFFFF
#define SIGNBIT_MASK32  0x80000000
#define MANTISSA_HIGHBIT_MASK32 0x00400000

C_Int_t Real32_class (Real32_t f) {
  uint32_t word0;
  int res;

  /* Using memcpy;
   * Technically correct.
   */
  uint32_t words[1];
  memcpy(&words, &f, sizeof(Real32_t));
  word0 = words[0];
  /* Using union;
   * Technically undefined, but widely supported.
   */
  /*
  union {float f; uint32_t words[1];} fws;
  fws.f = f;
  word0 = fws.words[0];
  */

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

/* This code assumes IEEE 754/854.
 *
 * In little-endian memory, the 64 bits of a double are layed out as follows.
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
 * In big-endian memory, the 64 bits of a double are layed out as follows.
 *
 * d[7]  bits 7-0 of mantissa
 * d[6]  bits 15-8 of mantissa
 * d[5]  bits 23-16 of mantissa
 * d[4]  bits 31-24 of mantissa
 * d[3]  bits 39-32 of mantissa
 * d[2]  bits 47-40 of mantissa
 * d[1]  bits 3-0 of exponent
 *       bits 51-48 of mantissa
 * d[0]  sign bit
 *       bits 10-4 of exponent
 */

/* masks for most-significant word */
#define EXPONENT_MASK64 0x7FF00000
#define MANTISSA_MASK64 0x000FFFFF
#define SIGNBIT_MASK64  0x80000000
#define MANTISSA_HIGHBIT_MASK64 0x00080000

C_Int_t Real64_class (Real64_t d) {
  uint32_t word0, word1;
  int res;

  /* Using memcpy;
   * Technically correct.
   */
  uint32_t words[2];
  memcpy(&words, &d, sizeof(Real64_t));
  if (isBigEndian()) {
    word1 = words[0];
    word0 = words[1];
  } else {
    word0 = words[0];
    word1 = words[1];
  }
  /* Using union;
   * Technically undefined, but widely supported.
   */
  /*
  union {double d; uint32_t words[2];} dws;
  dws.d = d;
  if (isBigEndian()) {
    word1 = dws.words[0];
    word0 = dws.words[1];
  } else {
    word0 = dws.words[0];
    word1 = dws.words[1];
  }
  */

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

#endif
