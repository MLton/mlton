#include "platform.h"

Real64_t Real64_modf (Real64_t x, Ref(Real64_t) exp);
Real64_t Real64_modf (Real64_t x, Ref(Real64_t) exp) {
  return modf (x, (Real64_t*)exp);
}

Real32_t Real32_modf (Real32_t x, Ref(Real32_t) exp);
Real32_t Real32_modf (Real32_t x, Ref(Real32_t) exp) {
  return modff (x, (Real32_t*)exp);
}
