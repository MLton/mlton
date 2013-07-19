#include "platform.h"
#include "gdtoa/gdtoa.h"

Real32_t Real32_strtor (NullString8_t s, C_Int_t rounding) {
  char *endptr;
  Real32_t res;

  gdtoa__strtorf ((const char*)s, &endptr, (int)rounding, &res);
  assert (NULL != endptr);
  return res;
}

Real64_t Real64_strtor (NullString8_t s, C_Int_t rounding) {
  char *endptr;
  Real64_t res;

  gdtoa__strtord ((const char*)s, &endptr, (int)rounding, &res);
  assert (NULL != endptr);
  return res;
}
