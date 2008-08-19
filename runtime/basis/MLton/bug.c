#include "platform.h"

/* print a bug message and exit (2) */
void MLton_bug (String8_t msg) {
  uintmax_t size = GC_getArrayLength ((pointer)msg);
  fprintf (stderr, "MLton bug: ");
  unless (0 == size)
    while (1 != fwrite ((const void*)msg, (size_t)size, 1, stderr))
      /* nothing */;
  fprintf (stderr, "\nPlease send a bug report to MLton@mlton.org.\n");
  exit (2);
}
