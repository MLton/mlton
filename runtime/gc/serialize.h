/* Copyright (C) 2013 KC Sivaramakrishnan.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define BASE_ADDR (pointer)0x10000000

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void serializeHelper (GC_state s, pointer msg, pointer dstBuffer, size_t msgSize);
pointer deserializeHelper (GC_state s, pointer bufferStart, size_t bufferSize);
PRIVATE pointer GC_serialize (GC_state s, pointer p, GC_header header);
PRIVATE pointer GC_deserialize (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
