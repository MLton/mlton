/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_thread copyThread (GC_state s, GC_thread from, size_t size) {
  GC_thread to;

  if (DEBUG_THREADS)
    fprintf (stderr, "copyThread ("FMTPTR")\n", (uintptr_t)from);
  /* newThread may do a GC, which invalidates from.
   * Hence we need to stash from where the GC can find it.
   */
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = pointerToObjptr((pointer)from - offsetofThread (s), s->heap.start);
  to = newThread (s, size);
  from = (GC_thread)(objptrToPointer(s->savedThread, s->heap.start) 
                     + offsetofThread (s));
  s->savedThread = BOGUS_OBJPTR;
  if (DEBUG_THREADS) {
    fprintf (stderr, FMTPTR" = copyThread ("FMTPTR")\n",
             (uintptr_t)to, (uintptr_t)from);
  }
  copyStack (s, 
             (GC_stack)(objptrToPointer(from->stack, s->heap.start)), 
             (GC_stack)(objptrToPointer(to->stack, s->heap.start)));
  to->bytesNeeded = from->bytesNeeded;
  to->exnStack = from->exnStack;
  return to;
}

void GC_copyCurrentThread (GC_state s) {
  GC_thread fromThread;
  GC_stack fromStack;
  GC_thread toThread;
  GC_stack toStack;

  if (DEBUG_THREADS)
    fprintf (stderr, "GC_copyCurrentThread\n");
  enter (s);
  fromThread = (GC_thread)(objptrToPointer(s->currentThread, s->heap.start) 
                           + offsetofThread (s));
  fromStack = (GC_stack)(objptrToPointer(fromThread->stack, s->heap.start));
  toThread = copyThread (s, fromThread, fromStack->used);
  toStack = (GC_stack)(objptrToPointer(toThread->stack, s->heap.start));
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  /* assert (fromStack->reserved == fromStack->used); */
  assert (fromStack->reserved >= fromStack->used);
  leave (s);
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = GC_copyCurrentThread\n", (uintptr_t)toThread);
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = pointerToObjptr((pointer)toThread - offsetofThread (s), s->heap.start);
}

pointer GC_copyThread (GC_state s, pointer p) {
  GC_thread fromThread;
  GC_stack fromStack;
  GC_thread toThread;
  GC_stack toStack;

  if (DEBUG_THREADS)
    fprintf (stderr, "GC_copyThread ("FMTPTR")\n", (uintptr_t)p);
  enter (s);
  fromThread = (GC_thread)(p + offsetofThread (s));
  fromStack = (GC_stack)(objptrToPointer(fromThread->stack, s->heap.start));
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  /* assert (fromStack->reserved == fromStack->used); */
  assert (fromStack->reserved >= fromStack->used);
  toThread = copyThread (s, fromThread, fromStack->used);
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  toStack = (GC_stack)(objptrToPointer(toThread->stack, s->heap.start));
  /* assert (fromStack->reserved == fromStack->used); */
  assert (fromStack->reserved >= fromStack->used);
  leave (s);
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = GC_copyThread ("FMTPTR")\n", 
             (uintptr_t)toThread, (uintptr_t)fromThread);
  return ((pointer)toThread - offsetofThread (s));
}
