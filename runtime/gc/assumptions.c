/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * Various assumptions about the underlying C translator.  This is the
 * place for characteristics that are not dictated by the C standard,
 * but which are reasonable to assume on a wide variety of target
 * platforms.  Working around these assumptions would be difficult.
 */
void checkAssumptions (void) {
  assert(CHAR_BIT == 8);
  /* assert(repof(uintptr_t) == TWOS); */
}
