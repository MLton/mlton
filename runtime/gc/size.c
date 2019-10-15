/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

size_t GC_size (GC_state s, pointer root) {
  struct GC_markState markState;
  size_t res;
  
  enter (s); /* update stack in heap, in case it is reached */
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size marking\n");
  markState.mode = MARK_MODE;
  markState.size = 0;
  markState.shouldHashCons = FALSE;
  markState.shouldLinkWeaks = FALSE;
  dfsMark (s, root, &markState);
  res = markState.size;
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_size unmarking\n");
  markState.mode = UNMARK_MODE;
  markState.size = 0;
  dfsMark (s, root, &markState);
  leave(s);

  return res;
}

size_t GC_sizeAll (GC_state s) {
  struct GC_foreachObjptrClosure dfsMarkObjptrClosure;
  struct GC_markState markState;
  size_t res;

  enter (s); /* update stack in heap, in case it is reached */
  dfsMarkObjptrClosure.fun = dfsMarkObjptrFun;
  dfsMarkObjptrClosure.env = &markState;
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeAll marking\n");
  markState.mode = MARK_MODE;
  markState.size = 0;
  markState.shouldHashCons = FALSE;
  markState.shouldLinkWeaks = FALSE;
  foreachGlobalObjptr (s, &dfsMarkObjptrClosure);
  res = markState.size;
  if (DEBUG_SIZE)
    fprintf (stderr, "GC_sizeAll unmarking\n");
  markState.mode = UNMARK_MODE;
  markState.size = 0;
  foreachGlobalObjptr (s, &dfsMarkObjptrClosure);
  leave(s);

  return res;
}
