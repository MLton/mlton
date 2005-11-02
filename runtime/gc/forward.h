/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_forwardState {
  bool amInMinorGC;
  pointer back;
  pointer toStart;
  pointer toLimit;
};

#define GC_FORWARDED ~((GC_header)0)

bool isPointerInToSpace (GC_state s, pointer p);
bool isObjptrsInToSpace (GC_state s, objptr op);
void forwardObjptr (GC_state s, objptr *opp);
void forwardObjptrIfInNursery (GC_state s, objptr *opp);
void forwardInterGenerationalObjptrs (GC_state s);
