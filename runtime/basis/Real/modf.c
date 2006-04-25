#include "platform.h"

#define binaryRealRealRef(g, h)                                 \
Real64_t Real64_##g (Real64_t x, Ref(Real64_t) yp) {            \
  return h (x, (Real64_t*)yp);                                  \
}                                                               \
Real32_t Real32_##g (Real32_t x, Ref(Real32_t) yp) {            \
  return h##f (x, (Real32_t*)yp);                               \
}
binaryRealRealRef(modf, modf)
#undef binaryRealRealRef
