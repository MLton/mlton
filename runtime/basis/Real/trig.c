#include <math.h>
#include "mlton-basis.h"

Real32 Real32_Math_cosh(Real32 x) {
  return (Real32)(cosh((Real64)x));
}

Real64 Real64_Math_cosh(Real64 x) {
  return cosh(x);
}

Real32 Real32_Math_sinh(Real32 x) {
  return (Real32)(sinh((Real64)x));
}

Real64 Real64_Math_sinh(Real64 x) {
  return sinh(x);
}

Real32 Real32_Math_tanh(Real32 x) {
  return (Real32)(tanh((Real64)x));
}

Real64 Real64_Math_tanh(Real64 x) {
  return tanh(x);
}
