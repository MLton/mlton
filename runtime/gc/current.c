/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline objptr currentThreadObjptr (GC_state s) {
  return s->currentThread;
}

static inline GC_thread currentThread (GC_state s) {
  pointer p = objptrToPointer(currentThreadObjptr(s), s->heap.start);
  return (GC_thread)p;
}

static inline objptr currentThreadStackObjptr (GC_state s) {
  GC_thread ct = currentThread (s);
  return ct->stack;
}

static inline GC_stack currentThreadStack (GC_state s) {
  pointer p = objptrToPointer(currentThreadStackObjptr(s), s->heap.start);
  return (GC_stack)p;
}

static inline size_t currentStackUsed (GC_state s) {
  return s->stackTop - s->stackBottom;
}



static void setCurrentStack (GC_state s) {
  GC_thread thread;
  GC_stack stack;
  
  thread = currentThread (s);
  s->exnStack = thread->exnStack;
  stack = currentThreadStack (s);
  s->stackBottom = stackBottom (s, stack);
  s->stackTop = stackTop (s, stack);
  s->stackLimit = stackLimit (s, stack);
  markCard (s, (pointer)stack);
}
