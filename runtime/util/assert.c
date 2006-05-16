/* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "util.h"

void asok (void) {
}

void asfail(const char *file, int line, const char *prop) {
  fflush(stdout);
  fprintf(stderr, "%s:%d: assert(%s) failed.\n", file, line, prop);
  abort();
}
