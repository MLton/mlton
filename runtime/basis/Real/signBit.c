#include "platform.h"

Int Real32_signBit (Real32 f) {
	int offset;

	offset = MLton_Platform_Arch_bigendian ? 0 : 3;
	return (((unsigned char *)&f)[offset] & 0x80) >> 7;
}

Int Real64_signBit (Real64 d) {
	int offset;

	offset = MLton_Platform_Arch_bigendian ? 0 : 7;
	return (((unsigned char *)&d)[offset] & 0x80) >> 7;
}
