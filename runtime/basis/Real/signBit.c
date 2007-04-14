#include "platform.h"

#if HAS_SIGNBIT

C_Int_t Real32_signBit (Real32_t f) {
  return signbit (f);
}

C_Int_t Real64_signBit (Real64_t d) {
  return signbit (d);
}

#else

enum {
  LITTLE_R32_byte = 3,
  LITTLE_R64_byte = 7,
};

enum {
  BIG_R32_byte = 0,
  BIG_R64_byte = 0,
};

C_Int_t Real32_signBit (Real32_t f) {
  int R32_byte;
  if (isBigEndian()) {
    R32_byte = BIG_R32_byte;
  } else {
    R32_byte = LITTLE_R32_byte;
  }

  /* Using memcpy. 
   * Technically correct.
   */
  unsigned char chars[4];
  memcpy(chars, &f, sizeof(Real32_t));
  return (chars[R32_byte] & 0x80) >> 7;
  /* Using cast; 
   * Technically correct, as (unsigned char*) may alias.   
   */
  /*
  return (((unsigned char*)(&f))[R32_byte] & 0x80) >> 7;
  */
  /* Using union; 
   * Technically undefined, but widely supported. 
   */
  /*
  union {float f; unsigned char c[4];} fc;
  fc.f = f;
  return (fc.c[R32_byte] & 0x80) >> 7;
  */
}

C_Int_t Real64_signBit (Real64_t d) {
  int R64_byte;
  if (isBigEndian()) {
    R64_byte = BIG_R64_byte;
  } else {
    R64_byte = LITTLE_R64_byte;
  }

  /* Using memcpy. 
   * Technically correct.
   */
  unsigned char chars[8];
  memcpy(chars, &d, sizeof(Real64_t));
  return (chars[R64_byte] & 0x80) >> 7;
  /* Using cast; 
   * Technically correct, as (unsigned char*) may alias.   
   */
  /*
  return (((unsigned char*)(&d))[R64_byte] & 0x80) >> 7;
  */
  /* Using union; 
   * Technically undefined, but widely supported. 
   */
  /*
  union {double d; unsigned char c[8];} dc;
  dc.d = d;
  return (dc.c[R64_byte] & 0x80) >> 7;
  */
}

#endif
