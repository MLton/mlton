/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofWeak (GC_state s) {
  size_t res;

  res = GC_NORMAL_METADATA_SIZE + sizeof (struct GC_weak);
  res = align (res, s->alignment);
  if (DEBUG) {
    size_t check;
    uint16_t bytesNonObjptrs, numObjptrs;

    splitHeader (s, GC_WEAK_GONE_HEADER, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
    check = GC_NORMAL_METADATA_SIZE + (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeofWeak: res = %"PRIuMAX"  check = %"PRIuMAX"\n",
               (uintmax_t)res, (uintmax_t)check);
    assert (check == res);
  }
  assert (isAligned (res, s->alignment));
  return res;
}

size_t offsetofWeak (GC_state s) {
  return (sizeofWeak (s)) - (GC_NORMAL_METADATA_SIZE + sizeof (struct GC_weak));
}

uint32_t GC_weakCanGet (__attribute__ ((unused)) GC_state s, pointer p) {
  uint32_t res;

  res = GC_WEAK_GONE_HEADER != getHeader (p);
  if (DEBUG_WEAK)
    fprintf (stderr, "%s = GC_weakCanGet ("FMTPTR")\n",
             boolToString (res), (uintptr_t)p);
  return res;
}

pointer GC_weakGet (GC_state s, pointer p) {
  GC_weak weak;
  pointer res;

  weak = (GC_weak)(p + offsetofWeak (s));
  res = objptrToPointer(weak->objptr, s->heap.start);
  if (DEBUG_WEAK)
    fprintf (stderr, FMTPTR" = GC_weakGet ("FMTPTR")\n",
             (uintptr_t)res, (uintptr_t)p);
  return res;
}

pointer GC_weakNew (GC_state s, GC_header header, pointer p) {
  GC_weak weak;
  pointer res;

  res = newObject (s, header,
                   sizeofWeak (s),
                   FALSE);
  weak = (GC_weak)(res + offsetofWeak (s));
  weak->objptr = pointerToObjptr(p, s->heap.start);
  if (DEBUG_WEAK)
    fprintf (stderr, FMTPTR" = GC_weakNew ("FMTHDR", "FMTPTR")\n",
             (uintptr_t)res, header, (uintptr_t)p);
  return (pointer)res;
}
