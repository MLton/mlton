#include "mlton-basis.h"

Int Real64_signBit (Real64 d) {
	return (((unsigned char *)&d)[7] & 0x80) >> 7;
}
