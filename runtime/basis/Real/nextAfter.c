#include <math.h>
#include "mlton-basis.h"

Real64 Real64_nextAfter (Real64 x1, Real64 x2) {
	return nextafter (x1, x2);
}
