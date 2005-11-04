/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void assertIsObjptrInFromSpace (GC_state s, objptr *opp);
bool invariantForGC (GC_state s);
bool invariantForMutatorFrontier (GC_state s);
bool invariantForMutatorStack (GC_state s);
bool invariantForMutator (GC_state s, bool frontier, bool stack);
