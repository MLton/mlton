#include "platform.h"

#define unaryReal(g, h)                                         \
Real64_t Real64_##g (Real64_t x);                               \
Real64_t Real64_##g (Real64_t x) {                              \
  return h (x);                                                 \
}                                                               \
Real32_t Real32_##g (Real32_t x);                               \
Real32_t Real32_##g (Real32_t x) {                              \
  return h##f (x);                                              \
}
unaryReal(abs, fabs)
unaryReal(round, rint)
#undef unaryReal

#define binaryReal(g, h)                                        \
Real64_t Real64_Math_##g (Real64_t x, Real64_t y);              \
Real64_t Real64_Math_##g (Real64_t x, Real64_t y) {             \
  return h (x, y);                                              \
}                                                               \
Real32_t Real32_Math_##g (Real32_t x, Real32_t y);              \
Real32_t Real32_Math_##g (Real32_t x, Real32_t y) {             \
  return h##f (x, y);                                           \
}
binaryReal(atan2, atan2)
binaryReal(pow, pow)
#undef binaryReal

#define unaryReal(g, h)                                         \
Real64_t Real64_##g (Real64_t x);                               \
Real64_t Real64_##g (Real64_t x) {                              \
  return h (x);                                                 \
}                                                               \
Real32_t Real32_##g (Real32_t x);                               \
Real32_t Real32_##g (Real32_t x) {                              \
  return h##f (x);                                              \
}
unaryReal(acos, acos)
unaryReal(asin, asin)
unaryReal(atan, atan)
unaryReal(cos, cos)
unaryReal(cosh, cosh)
unaryReal(exp, exp)
unaryReal(ln, log)
unaryReal(log10, log10)
unaryReal(sin, sin)
unaryReal(sinh, sinh)
unaryReal(sqrt, sqrt)
unaryReal(tan, tan)
unaryReal(tanh, tanh)
#undef unaryReal

Real64_t Real64_ldexp (Real64_t x, C_Int_t i);
Real64_t Real64_ldexp (Real64_t x, C_Int_t i) {
  return ldexp (x, i);
}

Real32_t Real32_ldexp (Real32_t x, C_Int_t i);
Real32_t Real32_ldexp (Real32_t x, C_Int_t i) {
  return ldexpf (x, i);
}
