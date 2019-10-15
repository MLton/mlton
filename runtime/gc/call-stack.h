/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct GC_callStackState {
  uint32_t numStackFrames;
  uint32_t *callStack;
} *GC_callStackState;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void numStackFramesAux (GC_state s,
                                      GC_frameIndex frameIndex,
                                      GC_frameInfo frameInfo,
                                      pointer frameTop,
                                      GC_callStackState callStackState);
static inline void numStackFramesFun (GC_state s,
                                      GC_frameIndex frameIndex,
                                      GC_frameInfo frameInfo,
                                      pointer frameTop,
                                      void *env);
static inline void callStackAux (GC_state s,
                                 GC_frameIndex frameIndex,
                                 GC_frameInfo frameInfo,
                                 pointer frameTop,
                                 GC_callStackState callStackState);
static inline void callStackFun (GC_state s,
                                 GC_frameIndex frameIndex,
                                 GC_frameInfo frameInfo,
                                 pointer frameTop,
                                 void *env);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE uint32_t GC_numStackFrames (GC_state s);
PRIVATE void GC_callStack (GC_state s, pointer p);
PRIVATE uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
