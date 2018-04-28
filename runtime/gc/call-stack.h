/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_callStackState {
  uint32_t numStackFrames;
  uint32_t *callStack;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void numStackFramesAux (GC_state s, GC_frameIndex i);
static inline void callStackAux (GC_state s, GC_frameIndex i);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE uint32_t GC_numStackFrames (GC_state s);
PRIVATE void GC_callStack (GC_state s, pointer p);
PRIVATE uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
