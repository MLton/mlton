#include "platform.h"

/* print a bug message and exit (2) */
void MLton_bug (NullString8_t msg) {
  fprintf (stderr, "MLton bug: %s.\n%s\n",
           (const char*)msg,
           "Please send a bug report to MLton@mlton.org.");
  exit (2);
}
