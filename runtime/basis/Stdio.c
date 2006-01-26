#include "platform.h"

void Stdio_print (String8_t s) {
  uintmax_t size = GC_getArrayLength ((pointer)s);
  if (0 == size)
    return;
  while (1 != fwrite ((const void*)s, (size_t)size, 1, stderr))
    /* nothing */;
}
