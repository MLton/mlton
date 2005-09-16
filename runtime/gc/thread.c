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

void displayThread (GC_state s,
                    GC_thread thread, 
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIu32"\n"
          "\t\tbytesNeeded = %"PRIu32"\n"
          "\t\tstack = "FMTOBJPTR"\n",
          thread->exnStack,
          thread->bytesNeeded,
          thread->stack);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, s->heap.start)),
                stream);
}
