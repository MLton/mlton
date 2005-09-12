/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* isPointer returns true if p looks like a pointer. */
static inline bool isPointer (pointer p) {
  uintptr_t mask = ~((~((uintptr_t)0)) << GC_MODEL_MINALIGN_SHIFT);
  return (0 == ((uintptr_t)p & mask));
}

static inline void GC_memcpy (pointer src, pointer dst, size_t size) {
  if (DEBUG_DETAILED)
    fprintf (stderr, "GC_memcpy ("FMTPTR", "FMTPTR", %zu)\n",
             (uintptr_t)src, (uintptr_t)dst, size);
  assert (isAligned ((uintptr_t)src, sizeof(unsigned int)));
  assert (isAligned ((uintptr_t)dst, sizeof(unsigned int)));
  assert (isAligned (size, sizeof(unsigned int)));
  assert (dst <= src or src + size <= dst);
  if (src == dst)
    return;
  memcpy (dst, src, size);
}
