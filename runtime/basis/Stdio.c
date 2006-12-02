#include "platform.h"

void Stdio_printStderr (String8_t s) {
  uintmax_t size = GC_getArrayLength ((pointer)s);
  if (0 == size)
    return;
  while (1 != fwrite ((const void*)s, (size_t)size, 1, stderr))
    /* nothing */;
}

void Stdio_printStdout (String8_t s) {
  uintmax_t size = GC_getArrayLength ((pointer)s);
  if (0 == size)
    return;
  while (1 != fwrite ((const void*)s, (size_t)size, 1, stdout))
    /* nothing */;
}

void Stdio_print (String8_t s) {
  Stdio_printStderr (s);
}
