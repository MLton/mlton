/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline GC_thread copyThread (GC_state s, GC_thread from, size_t size);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_copyCurrentThread (GC_state s);
pointer GC_copyThread (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
