#include <math.h>
#include "mlton-basis.h"

Real32 Real32_copysign (Real32 f1, Real32 f2) {
	return copysignf (f1, f2); /* copysignf is from math.h */
}

Real64 Real64_copysign (Real64 d1, Real64 d2) {
	return copysign (d1, d2); /* copysign is from math.h */
}
