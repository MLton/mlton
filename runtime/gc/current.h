/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline objptr getThreadCurrentObjptr (GC_state s);
static inline GC_thread getThreadCurrent (GC_state s);
static inline objptr getStackCurrentObjptr (GC_state s);
static inline GC_stack getStackCurrent (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
