#include "platform.h"

Int Real64_class (Real64 d) {
	return fpclassify32 (d);
}

Int Real32_class (Real32 f) {
	return fpclassify64 (f);
}
