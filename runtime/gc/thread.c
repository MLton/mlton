/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline GC_thread currentThread (GC_state s) {
  pointer p = objptrToPointer(s->currentThread, s->heap.start);
  return (GC_thread)p;
}

static inline objptr currentThreadStack (GC_state s) {
  GC_thread ct = currentThread (s);
  return ct->stack;
}
