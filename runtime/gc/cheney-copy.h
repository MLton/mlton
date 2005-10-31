/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

bool pointerIsInToSpace (pointer p);
bool objptrIsInToSpace (objptr op);
void forwardObjptr (GC_state s, objptr *opp);
void updateWeaks (GC_state s);
void swapHeaps (GC_state s);
void majorCheneyCopyGC (GC_state s);
void forwardObjptrIfInNursery (GC_state s, objptr *opp);
void forwardInterGenerationalObjptrs (GC_state s);
void minorCheneyCopyGC (GC_state s);
