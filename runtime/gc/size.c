/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

size_t GC_size (GC_state s, pointer root) {
  size_t res;
  
  enter (s); /* update stack in heap, in case it is reached */
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size marking\n");
  s->markState.mode = MARK_MODE;
  s->markState.size = 0;
  s->markState.shouldHashCons = FALSE;
  s->markState.shouldLinkWeaks = FALSE;
  dfsMark (s, root);
  res = s->markState.size;
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size unmarking\n");
  s->markState.mode = UNMARK_MODE;
  dfsMark (s, root);
  leave(s);
  
  return res;
}
