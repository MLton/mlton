#include <math.h>
#include "mlton-basis.h"

Int Real_qequal (Double x1, Double x2) {
	return Real_isNan (x1) || Real_isNan (x2) || x1 == x2;
}
