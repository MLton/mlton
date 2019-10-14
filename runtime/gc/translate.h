/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void translateFun (GC_state s, objptr *opp, void *env);
static void translateHeap (GC_state s, pointer from, pointer to, size_t size);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
