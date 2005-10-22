/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


/* GC_init uses the array of struct intInfInits in s at program start
 * to allocate intInfs.  
 * The globalIndex'th entry of the globals array in s is set to the
 * IntInf.int whose value corresponds to the mlstr string.
 *
 * The strings pointed to by the mlstr fields consist of
 *      an optional ~
 *      either one or more of [0-9] or
 *             0x followed by one or more of [0-9a-fA-F]
 *      a trailing EOS
 */
struct GC_intInfInit {
  uint32_t globalIndex;
  char *mlstr;
};

/* GC_init allocates a collection of arrays/vectors in the heap. */
struct GC_vectorInit {
  pointer bytes;
  size_t bytesPerElement;
  uint32_t globalIndex;
  GC_arrayLength numElements;
};
