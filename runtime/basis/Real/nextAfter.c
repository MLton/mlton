#include <math.h>
#include "mlton-basis.h"

Double Real_nextAfter (Double x1, Double x2) {
	return nextafter (x1, x2);
}
