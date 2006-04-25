#include "platform.h"

/* nextafter is a macro, so we must have a C wrapper to work correctly. */
Real32_t Real32_nextAfter (Real32_t x1, Real32_t x2);
Real32_t Real32_nextAfter (Real32_t x1, Real32_t x2) {
  return nextafterf (x1, x2);
}

Real64_t Real64_nextAfter (Real64_t x1, Real64_t x2);
Real64_t Real64_nextAfter (Real64_t x1, Real64_t x2) {
  return nextafter (x1, x2);
}
