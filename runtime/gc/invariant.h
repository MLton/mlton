/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void assertIsObjptrInFromSpace (GC_state s, objptr *opp);
static bool invariantForGC (GC_state s);
static inline bool invariantForMutatorFrontier (GC_state s);
static inline bool invariantForMutatorStack (GC_state s);
static bool invariantForMutator (GC_state s, bool frontier, bool stack);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
