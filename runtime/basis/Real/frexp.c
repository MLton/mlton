#include "platform.h"

Real32_t Real32_frexp (Real32_t x, Ref(C_Int_t) exp);
Real32_t Real32_frexp (Real32_t x, Ref(C_Int_t) exp) {
  return frexpf (x, (int*)exp);
}

Real64_t Real64_frexp (Real64_t x, Ref(C_Int_t) exp);
Real64_t Real64_frexp (Real64_t x, Ref(C_Int_t) exp) {
  return frexp (x, (int*)exp);
}
