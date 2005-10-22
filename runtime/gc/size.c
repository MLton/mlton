/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t GC_size (GC_state s, pointer root) {
  size_t res;

  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size marking\n");
  res = dfsMark (s, root, MARK_MODE, FALSE);
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size unmarking\n");
  dfsMark (s, root, UNMARK_MODE, FALSE);
  return res;
}
