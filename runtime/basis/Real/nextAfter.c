#include <math.h>
#include "mlton-basis.h"

Real32 Real32_nextAfter (Real32 x1, Real32 x2) {
	return nextafterf (x1, x2);
}

Real64 Real64_nextAfter (Real64 x1, Real64 x2) {
	return nextafter (x1, x2);
}
