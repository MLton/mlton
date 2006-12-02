#include "platform.h"

#if HAS_SIGNBIT

C_Int_t Real32_signBit (Real32_t f) {
  return signbit (f);
}

C_Int_t Real64_signBit (Real64_t d) {
  return signbit (d);
}

#else

#if (defined __i386__)

enum {
  R32_byte = 3,
  R64_byte = 7,
};

#elif (defined __ppc__ || defined __sparc__)

enum {
  R32_byte = 0,
  R64_byte = 0,
};

#else

#error Real_signBit not implemented

#endif

C_Int_t Real32_signBit (Real32_t f) {
  return (((unsigned char *)&f)[R32_byte] & 0x80) >> 7;
}

C_Int_t Real64_signBit (Real64_t d) {
  return (((unsigned char *)&d)[R64_byte] & 0x80) >> 7;
}

#endif
