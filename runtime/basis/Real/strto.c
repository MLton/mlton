#include "platform.h"
#include "gdtoa/gdtoa.h"

Real32_t Real32_strto (NullString8_t s) {
  char *endptr;
  Real32_t res;

  res = gdtoa_strtof ((const char*)s, &endptr);
  assert (NULL != endptr);
  return res;
}

Real64_t Real64_strto (NullString8_t s) {
  char *endptr;
  Real64_t res;

  res = gdtoa_strtod ((const char*)s, &endptr);
  assert (NULL != endptr);
  return res;
}
