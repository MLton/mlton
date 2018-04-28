/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

objptr getThreadCurrentObjptr (GC_state s) {
  return s->currentThread;
}

GC_thread getThreadCurrent (GC_state s) {
  pointer p = objptrToPointer(getThreadCurrentObjptr(s), s->heap.start);
  return (GC_thread)(p + offsetofThread (s));
}

objptr getStackCurrentObjptr (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return thread->stack;
}

GC_stack getStackCurrent (GC_state s) {
  pointer p = objptrToPointer(getStackCurrentObjptr(s), s->heap.start);
  return (GC_stack)p;
}
