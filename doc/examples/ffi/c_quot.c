#include "test_quot.h"
#include <stdio.h>

Int8 c_quot(Int8 x, Int8 y) {
  Int8 z = x / y;
  return z;
}

void call_sml_quot() {
  Int8 x = -1;
  Int8 y = 10;
  Int8 z = sml_quot(x, y);
  printf(" sml_z = %i\n", z);
}
