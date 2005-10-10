/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline bool isAligned (size_t a, size_t b) {
  return 0 == a % b;
}

static inline size_t alignDown (size_t a, size_t b) {
  assert (b >= 1);
  a -= a % b;
  assert (isAligned (a, b));
  return a;
}

static inline size_t align (size_t a, size_t b) {
  assert (b >= 1);
  a += b - 1;
  a -= a % b;
  assert (isAligned (a, b));
  return a;       
}

static inline size_t pad (GC_state s, size_t bytes, size_t extra) {
  return align (bytes + extra, s->alignment) - extra;
}

#if ASSERT
static inline bool isAlignedFrontier (GC_state s, pointer p) {
  return isAligned ((size_t)p + GC_NORMAL_HEADER_SIZE, 
                    s->alignment);
}
#endif

static inline pointer alignFrontier (GC_state s, pointer p) {
  size_t res;

  res = pad (s, (size_t)p, GC_NORMAL_HEADER_SIZE);
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR" = stackReserved ("FMTPTR")\n", 
             (uintptr_t)p, (uintptr_t)res);
  assert (isAlignedFrontier (s, (pointer)res));
  return (pointer)res;
}

pointer GC_alignFrontier (GC_state s, pointer p) {
  return alignFrontier (s, p);
}

#if ASSERT
static inline bool isAlignedStackReserved (GC_state s, size_t reserved) {
  return isAligned (GC_STACK_HEADER_SIZE + sizeof (struct GC_stack) + reserved, 
                    s->alignment);
}
#endif

static inline size_t alignStackReserved (GC_state s, size_t reserved) {
  size_t res;
  
  res = pad (s, reserved, GC_STACK_HEADER_SIZE + sizeof (struct GC_stack));
  if (DEBUG_STACKS)
    fprintf (stderr, "%zu = stackReserved (%zu)\n", res, reserved);
  assert (isAlignedStackReserved (s, res));
  return res;
}
