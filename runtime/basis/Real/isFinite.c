#include <math.h>
#include "mlton-basis.h"

Int Real_isFinite (Double d) {
	return finite (d); /* finite is from math.h */
}
