#include "platform.h"
#include "gdtoa/gdtoa.h"

Real32_t Real32_strto (NullString8_t s, C_Int_t rounding) {
  char *endptr;
  Real32_t res;
  int ret;

  ret = gdtoa__strtorf ((const char*)s, &endptr, (int)rounding, &res);
  assert (NULL != endptr);
  return res;
}

Real64_t Real64_strto (NullString8_t s, C_Int_t rounding) {
  char *endptr;
  Real64_t res;
  int ret;

  ret = gdtoa__strtord ((const char*)s, &endptr, (int)rounding, &res);
  assert (NULL != endptr);
  return res;
}
