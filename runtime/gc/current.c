/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

objptr getThreadCurrentObjptr (GC_state s) {
  return s->currentThread;
}

GC_thread getThreadCurrent (GC_state s) {
  pointer p = objptrToPointer(getThreadCurrentObjptr(s), s->heap.start);
  return (GC_thread)p;
}

objptr getStackCurrentObjptr (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return thread->stack;
}

GC_stack getStackCurrent (GC_state s) {
  pointer p = objptrToPointer(getStackCurrentObjptr(s), s->heap.start);
  return (GC_stack)p;
}

size_t sizeofStackCurrentUsed (GC_state s) {
  return s->stackTop - s->stackBottom;
}



void setThreadAndStackCurrent (GC_state s) {
  GC_thread thread;
  GC_stack stack;
  
  thread = getThreadCurrent (s);
  s->exnStack = thread->exnStack;
  stack = getStackCurrent (s);
  s->stackBottom = getStackBottom (s, stack);
  s->stackTop = getStackTop (s, stack);
  s->stackLimit = getStackLimit (s, stack);
  markCard (s, (pointer)stack);
}
