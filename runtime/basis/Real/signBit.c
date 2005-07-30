#include "platform.h"

Int Real32_signBit (Real32 f) {
	return signbit32 (f);
}

Int Real64_signBit (Real64 d) {
	return signbit64 (d);
}
