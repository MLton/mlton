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

Int signbit32 (Real32 f) {
        return (((unsigned char *)&f)[R32_byte] & 0x80) >> 7;
}

Int signbit64 (Real64 d) {
        return (((unsigned char *)&d)[R64_byte] & 0x80) >> 7;
}
