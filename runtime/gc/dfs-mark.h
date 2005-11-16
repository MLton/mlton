/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef enum {
  MARK_MODE,
  UNMARK_MODE,
} GC_markMode;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerMarked (pointer p);
static inline bool isPointerMarkedByMode (pointer p, GC_markMode m);
static size_t dfsMarkByMode (GC_state s, pointer root,
                             GC_markMode mode, bool shouldHashCons);
static inline void dfsMarkWithHashCons (GC_state s, objptr *opp);
static inline void dfsMarkWithoutHashCons (GC_state s, objptr *opp);
static inline void dfsUnmark (GC_state s, objptr *opp);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
