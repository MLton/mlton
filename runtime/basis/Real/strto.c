#include "platform.h"

Real32_t gdtoa_strtof (char *s, char **endptr);
Real64_t gdtoa_strtod (char *s, char **endptr);

Real32_t Real32_strto (NullString8_t s);
Real32_t Real32_strto (NullString8_t s) {
  char *endptr;
  Real32_t res;
  
  res = gdtoa_strtof ((char*)s, &endptr);
  assert (NULL != endptr);
  return res;
}

Real64_t Real64_strto (NullString8_t s);
Real64_t Real64_strto (NullString8_t s) {
  char *endptr;
  Real64 res;
  
  res = gdtoa_strtod ((char*)s, &endptr);
  assert (NULL != endptr);
  return res;
}
