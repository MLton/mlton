/* Copyright (C) 2010,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void *GC_mmapAnon_safe (void *p, size_t length) {
  void *result;

  result = GC_mmapAnon (p, length);
  if ((void*)-1 == result) {
    GC_displayMem ();
    die ("Out of memory.  Unable to allocate %s bytes.\n",
         uintmaxToCommaString(length));
  }
  return result;
}

static inline void GC_memcpy (pointer src, pointer dst, size_t size) {
  if (DEBUG_DETAILED)
    fprintf (stderr, "GC_memcpy ("FMTPTR", "FMTPTR", %"PRIuMAX")\n",
             (uintptr_t)src, (uintptr_t)dst, (uintmax_t)size);
  assert (! (src <= dst and dst < src + size));
  assert (! (dst <= src and src < dst + size));
  memcpy (dst, src, size);
}

static inline void GC_memmove (pointer src, pointer dst, size_t size) {
  if (DEBUG_DETAILED)
    fprintf (stderr, "GC_memmove ("FMTPTR", "FMTPTR", %"PRIuMAX")\n",
             (uintptr_t)src, (uintptr_t)dst, (uintmax_t)size);
  if (src == dst)
    return;
  memmove (dst, src, size);
}
