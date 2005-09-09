/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline size_t align (size_t a, size_t b) {
  assert (a >= 0);
  assert (b >= 1);
  a += b - 1;
  a -= a % b;
  return a;       
}

/*
static inline W64 w64align (W64 a, uint b) {
        W64 res;

        assert (a >= 0);
        assert (b >= 1);
        res = a + b - 1;
        res = res - res % b;
        if (FALSE)
                fprintf (stderr, "%llu = w64Align (%llu, %u)\n", res, a, b);
        return res;
}
*/

static inline bool isAligned (uintptr_t a, size_t b) {
  return 0 == a % b;
}

#if ASSERT
static inline bool isAlignedFrontier (GC_state s, pointer p) {
  return isAligned ((uintptr_t)p + GC_NORMAL_HEADER_SIZE, s->alignment);
}

/*
static bool isAlignedReserved (GC_state s, uint r) {
  return isAligned (STACK_HEADER_SIZE + sizeof (struct GC_stack) + r, 
                    s->alignment);
}
*/
#endif

static inline size_t pad (GC_state s, size_t bytes, size_t extra) {
  return align (bytes + extra, s->alignment) - extra;
}

static inline pointer alignFrontier (GC_state s, pointer p) {
  return (pointer) pad (s, (size_t)p, GC_NORMAL_HEADER_SIZE);
}

pointer GC_alignFrontier (GC_state s, pointer p) {
  return alignFrontier (s, p);
}

/*
static inline uint stackReserved (GC_state s, uint r) {
  uint res;
  
  res = pad (s, r, STACK_HEADER_SIZE + sizeof (struct GC_stack));
  if (DEBUG_STACKS)
    fprintf (stderr, "%s = stackReserved (%s)\n",
             uintToCommaString (res),
             uintToCommaString (r));
  return res;
}
*/
