/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void assertObjptrIsInFromSpace (GC_state s, objptr *opp);
bool invariant (GC_state s);
bool mutatorFrontierInvariant (GC_state s);
bool mutatorStackInvariant (GC_state s);
bool mutatorInvariant (GC_state s, bool frontier, bool stack);
