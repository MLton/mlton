#include "platform.h"

/* print a bug message and exit (2) */
void MLton_bug (String8_t msg) {
  uintmax_t size = GC_getSequenceLength ((pointer)msg);
  fprintf (stderr, "MLton bug: ");
  fwrite ((const void*)msg, (size_t)size, 1, stderr);
  fprintf (stderr, "\nPlease send a bug report to MLton@mlton.org.\n");
  exit (2);
}
