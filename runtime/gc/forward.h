/* Copyright (C) 2012,2016,2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct GC_forwardState {
  bool amInMinorGC;
  pointer back;
  pointer toStart;
  pointer toLimit;
} *GC_forwardState;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#if ASSERT
static inline bool isPointerInToSpace (GC_state s, pointer p, GC_forwardState forwardState);
static inline bool isObjptrInToSpace (GC_state s, objptr opp, GC_forwardState forwardState);
#endif

static inline objptr* getFwdPtrp (pointer p);
static inline objptr getFwdPtr (pointer p);
static inline bool hasFwdPtr (pointer p);
static inline void forwardObjptr (GC_state s, objptr *oppp, GC_forwardState forwardState);
static inline void forwardObjptrIfInHeap (GC_state s, objptr *oppp, GC_forwardState forwardState);
static inline void forwardObjptrIfInHeapFun (GC_state s, objptr *oppp, void *env);
static inline void forwardObjptrIfInNursery (GC_state s, objptr *oppp, GC_forwardState forwardState);
static inline void forwardObjptrIfInNurseryFun (GC_state s, objptr *oppp, void *env);
static inline void forwardInterGenerationalObjptrs (GC_state s, GC_forwardState forwardState);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
