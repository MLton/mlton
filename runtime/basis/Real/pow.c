#include <math.h>
#include "mlton-basis.h"

Real32 Real32_Math_pow(Real32 x, Real32 y) {
  return (Real32)(pow((Real64)x, (Real64)y));
}

Real64 Real64_Math_pow(Real64 x, Real64 y) {
  return pow(x, y);
}
