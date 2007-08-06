#include "platform.h"
#include "gdtoa/gdtoa.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

/* This code is patterned on g_dfmt from the gdtoa sources. */
C_String_t Real32_gdtoa (Real32_t f, C_Int_t mode, C_Int_t ndig, 
                         C_Int_t rounding, Ref(C_Int_t) decpt) {
  ULong bits[1];
  int ex;
  FPI fpi = { 24, 1-127-24+1, 254-127-24+1, (int)rounding, 0 };
  int i;
  ULong L[1];
  char *result;
  ULong sign;

  memcpy(L, &f, sizeof(Real32_t));
  sign = L[0] & 0x80000000L;
  bits[0] = L[0] & 0x7fffff;
  if (0 != (ex = (L[0] >> 23) & 0xff))
    bits[0] |= 0x800000;
  else
    ex = 1;
  ex -= 0x7f + 23;
  i = STRTOG_Normal;
  result = gdtoa__gdtoa (&fpi, ex, bits, &i, (int)mode, (int)ndig, (int*)decpt, NULL);
  if (DEBUG)
    fprintf (stderr, "%s = gdtoa (%g, %d, %d, %d)   decpt = %d\n", 
             result, (double)f, (int)mode, (int)ndig, (int)rounding, *((int*)decpt));
  return (C_String_t)result;
}

C_String_t Real64_gdtoa (Real64_t d, C_Int_t mode, C_Int_t ndig, 
                         C_Int_t rounding, Ref(C_Int_t) decpt) {
  ULong bits[2];
  int ex;
  FPI fpi = { 53, 1-1023-53+1, 2046-1023-53+1, (int)rounding, 0 };
  int i;
  ULong L[2];
  char *result;
  ULong sign;
  int x0, x1;

  if (isBigEndian()) {
    x0 = 0;
    x1 = 1;
  } else {
    x0 = 1;
    x1 = 0;
  }
  memcpy(L, &d, sizeof(Real64_t));
  sign = L[x0] & 0x80000000L;
  bits[0] = L[x1];
  bits[1] = L[x0] & 0xfffff;
  if (0 != (ex = (L[x0] >> 20) & 0x7ff))
    bits[1] |= 0x100000;
  else
    ex = 1;
  ex -= 0x3ff + 52;
  i = STRTOG_Normal;
  result = gdtoa__gdtoa (&fpi, ex, bits, &i, mode, ndig, (int*)decpt, NULL);
  if (DEBUG)
    fprintf (stderr, "%s = gdtoa (%g, %d, %d, %d)   decpt = %d\n", 
             result, d, (int)mode, (int)ndig, (int)rounding, *((int*)decpt));
  return (C_String_t)result;
}
