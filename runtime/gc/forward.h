/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_forwardState {
  bool amInMinorGC;
  pointer back;
  pointer toStart;
  pointer toLimit;
};

#define GC_FORWARDED ~((GC_header)0)

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static bool isPointerInToSpace (GC_state s, pointer p);
static bool isObjptrInToSpace (GC_state s, objptr op);
static void forwardObjptr (GC_state s, objptr *opp);
static void forwardObjptrIfInNursery (GC_state s, objptr *opp);
static void forwardInterGenerationalObjptrs (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
