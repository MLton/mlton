#include <math.h>
#include "basis-constants.h"
#include "mlton-basis.h"

Double Real_Math_pi = M_PI;
Double Real_Math_e = M_E;

#if (defined __sparc__)

double Real_maxFinite =    1.7976931348623157e308;
double Real_minPos =       4.94065645841246544e-324;
double Real_minNormalPos = 2.22507385850720140e-308;

#endif
