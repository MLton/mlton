#include "platform.h"

#define binaryRealIntRef(g, h)                                  \
Real64_t Real64_##g (Real64_t x, Ref(C_Int_t) i) {              \
  return h (x, (int*)i);                                        \
}                                                               \
Real32_t Real32_##g (Real32_t x, Ref(C_Int_t) i) {              \
  return h##f (x, (int*)i);                                     \
}
binaryRealIntRef(frexp, frexp)
#undef binaryRealIntRef
