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

bool isMarked (pointer p);
bool isMarkedMode (GC_markMode m, pointer p);
pointer arrayIndexAtPointer (GC_state s,
                             pointer a,
                             GC_arrayCounter arrayIndex,
                             uint32_t pointerIndex);
size_t dfsMark (GC_state s, pointer root,
                GC_markMode mode, bool shouldHashCons);
void dfsMarkTrue (GC_state s, objptr *opp);
void dfsMarkFalse (GC_state s, objptr *opp);
void dfsUnmark (GC_state s, objptr *opp);
