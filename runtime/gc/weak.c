/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


uint32_t GC_weakCanGet (GC_state s, pointer p) {
  uint32_t res;

  res = GC_WEAK_GONE_HEADER != getHeader (p);
  if (DEBUG_WEAK)
    fprintf (stderr, "%s = GC_weakCanGet ("FMTPTR")\n",
             boolToString (res), (uintptr_t)p);
  return res;
}

pointer GC_weakGet (GC_state s, pointer p) {
  pointer res;

  res = objptrToPointer(((GC_weak)p)->objptr, s->heap.start);
  if (DEBUG_WEAK)
    fprintf (stderr, FMTPTR" = GC_weakGet ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)p);
  return res;
}

pointer GC_weakNew (GC_state s, GC_header header, pointer p) {
  pointer res;

  res = newObject (s, header, 
                   GC_NORMAL_HEADER_SIZE + sizeof(struct GC_weak), 
                   FALSE);
  ((GC_weak)res)->objptr = pointerToObjptr(p, s->heap.start);
  if (DEBUG_WEAK)
    fprintf (stderr, FMTPTR" = GC_weakNew ("FMTHDR", "FMTPTR")\n",
             (uintptr_t)res, header, (uintptr_t)p);
  return res;
}
