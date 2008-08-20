/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
  assert (isAligned ((size_t)src, sizeof(unsigned int)));
  assert (isAligned ((size_t)dst, sizeof(unsigned int)));
  assert (isAligned (size, sizeof(unsigned int)));
  assert (dst <= src or src + size <= dst);
  if (src == dst)
    return;
  memcpy (dst, src, size);
}
