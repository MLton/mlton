/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void assertIsObjptrInFromSpace (GC_state s, objptr *opp);
#if ASSERT
static bool invariantForGC (GC_state s);
#endif
static inline bool invariantForMutatorFrontier (GC_state s);
static inline bool invariantForMutatorStack (GC_state s);
#if ASSERT
static bool invariantForMutator (GC_state s, bool frontier, bool stack);
#endif

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
