#define _ISOC99_SOURCE

#include "platform.h"

/* nextafter is a macro, so we must have a C wrapper to work correctly. */
Real64 Real64_nextAfter (Real64 x1, Real64 x2) {
	return nextafter (x1, x2);
}
