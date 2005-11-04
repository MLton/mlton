/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef enum {
  MARK_MODE,
  UNMARK_MODE,
} GC_markMode;

bool isPointerMarked (pointer p);
bool isPointerMarkedByMode (pointer p, GC_markMode m);
size_t dfsMarkByMode (GC_state s, pointer root,
                      GC_markMode mode, bool shouldHashCons);
void dfsMarkWithHashCons (GC_state s, objptr *opp);
void dfsMarkWithoutHashCons (GC_state s, objptr *opp);
void dfsUnmark (GC_state s, objptr *opp);
