#include "platform.h"

#if HAS_SIGNBIT

Int Real32_signBit (Real32 f) {
        return signbit (f);
}

Int Real64_signBit (Real64 d) {
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

Int Real32_signBit (Real32 f) {
        return (((unsigned char *)&f)[R32_byte] & 0x80) >> 7;
}

Int Real64_signBit (Real64 d) {
        return (((unsigned char *)&d)[R64_byte] & 0x80) >> 7;
}

#endif
