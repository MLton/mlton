/* Copyright (C) 2012,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef enum {
  MARK_MODE,
  UNMARK_MODE,
} GC_markMode;

struct GC_markState {
  GC_markMode mode;
  bool shouldHashCons;
  bool shouldLinkWeaks;
  size_t size;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerMarked (pointer p);
static inline bool isPointerMarkedByMode (pointer p, GC_markMode m);
static void dfsMark (GC_state s, pointer root);
static void dfsMarkObjptr (GC_state s, objptr *root, void *env);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
