#include <math.h>
#include "mlton-basis.h"

Int Real32_signBit (Real32 f) {
	return (((unsigned char *)&f)[3] & 0x80) >> 7;
}

Int Real64_signBit (Real64 d) {
	return (((unsigned char *)&d)[7] & 0x80) >> 7;
}
