/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* GC_init uses the array of struct intInfInits in s at program start
 * to allocate intInfs.  
 * The globalIndex'th entry of the globals array in s is set to the
 * IntInf.int whose value corresponds to the mlstr string.
 *
 * The strings pointed to by the mlstr fields consist of
 *      an optional ~
 *      one of [1-9]
 *      zero or more of [0-9]
 *      a trailing EOS
 */
struct GC_intInfInit {
  uint32_t globalIndex;
  const char *mlstr;
};

/* GC_init allocates a collection of arrays/vectors in the heap. */
struct GC_vectorInit {
  pointer bytes;
  size_t bytesPerElement;
  uint32_t globalIndex;
  GC_arrayLength numElements;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline size_t sizeofIntInfFromString (GC_state s, const char *str);
static inline size_t sizeofInitialBytesLive (GC_state s);
static void initIntInfs (GC_state s);
static void initVectors (GC_state s);
static void initWorld (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
