#include "mlton-basis.h"

Int Real_signBit (Double d) {
	return (((unsigned char *)&d)[7] & 0x80) >> 7;
}
