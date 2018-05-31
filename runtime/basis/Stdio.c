#include "platform.h"

void Stdio_printStderr (String8_t s) {
  uintmax_t size = GC_getSequenceLength ((pointer)s);
  fwrite ((const void*)s, (size_t)size, 1, stderr);
}

void Stdio_printStdout (String8_t s) {
  uintmax_t size = GC_getSequenceLength ((pointer)s);
  fwrite ((const void*)s, (size_t)size, 1, stdout);
}

void Stdio_print (String8_t s) {
  Stdio_printStderr (s);
}
