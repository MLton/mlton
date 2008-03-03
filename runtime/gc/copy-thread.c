/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

GC_thread copyThread (GC_state s, GC_thread from, size_t size) {
  GC_thread to;

  if (DEBUG_THREADS or s->controls->messages)
    fprintf (stderr, "copyThread ("FMTPTR")\n", (uintptr_t)from);
  /* newThread may do a GC, which invalidates from.
   * Hence we need to stash from where the GC can find it.
   */
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = pointerToObjptr((pointer)from - offsetofThread (s), s->heap->start);
  to = newThread (s, size);
  from = (GC_thread)(objptrToPointer(s->savedThread, s->heap->start) 
                     + offsetofThread (s));
  s->savedThread = BOGUS_OBJPTR;
  if (DEBUG_THREADS) {
    fprintf (stderr, FMTPTR" = copyThread ("FMTPTR")\n",
             (uintptr_t)to, (uintptr_t)from);
  }
  copyStack (s, 
             (GC_stack)(objptrToPointer(from->stack, s->heap->start)), 
             (GC_stack)(objptrToPointer(to->stack, s->heap->start)));
  to->bytesNeeded = from->bytesNeeded;
  to->exnStack = from->exnStack;
  return to;
}

void GC_copyCurrentThread (GC_state s) {
  GC_thread fromThread;
  GC_stack fromStack;
  GC_thread toThread;
  GC_stack toStack;

  if (DEBUG_THREADS or s->controls->messages)
    fprintf (stderr, "GC_copyCurrentThread [%d]\n", Proc_processorNumber (s));

  /* Used to be an ENTER here, but we don't really need to synchronize unless
     we don't have enough room to allocate a new thread and stack. */
  
  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  fromThread = (GC_thread)(objptrToPointer(s->currentThread, s->heap->start) 
                           + offsetofThread (s));
  fromStack = (GC_stack)(objptrToPointer(fromThread->stack, s->heap->start));
  toThread = copyThread (s, fromThread, fromStack->reserved);

  /* Look up these again since a GC may have occurred and moved them */
  fromThread = (GC_thread)(objptrToPointer(s->currentThread, s->heap->start) 
                           + offsetofThread (s));
  fromStack = (GC_stack)(objptrToPointer(fromThread->stack, s->heap->start));
  toStack = (GC_stack)(objptrToPointer(toThread->stack, s->heap->start));
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  /* assert (fromStack->reserved == fromStack->used); */
  assert (fromStack->reserved >= fromStack->used);

  /* Formerly: LEAVE1 (s, "toThread"); */

  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = GC_copyCurrentThread [%d]\n", 
             (uintptr_t)toThread, Proc_processorNumber (s));
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = pointerToObjptr((pointer)toThread - offsetofThread (s), s->heap->start);
}

pointer GC_copyThread (GC_state s, pointer p) {
  GC_thread fromThread;
  GC_stack fromStack;
  GC_thread toThread;
  //GC_stack toStack;

  if (DEBUG_THREADS)
    fprintf (stderr, "GC_copyThread ("FMTPTR") [%d]\n", (uintptr_t)p,
             Proc_processorNumber (s));

  /* Used to be an ENTER here, but we don't really need to synchronize unless
     we don't have enough room to allocate a new thread and stack. */

  /* XXX copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  fromThread = (GC_thread)(p + offsetofThread (s));
  fromStack = (GC_stack)(objptrToPointer(fromThread->stack, s->heap->start));
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  /* assert (fromStack->reserved == fromStack->used); */
  assert (fromStack->reserved >= fromStack->used);
  toThread = copyThread (s, fromThread, fromStack->reserved);
  /* The following assert is no longer true, since alignment
   * restrictions can force the reserved to be slightly larger than
   * the used.
   */
  //toStack = (GC_stack)(objptrToPointer(toThread->stack, s->heap->start));
  /* assert (fromStack->reserved == fromStack->used); */
  /* Can't trust fromStack to be set properly (i.e. after GC). */
  //assert (fromStack->reserved >= fromStack->used);

  /* Formerly: LEAVE2 (s, "toThread", "fromThread"); */

  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = GC_copyThread ("FMTPTR") [%d]\n", 
             (uintptr_t)toThread, (uintptr_t)fromThread,
             Proc_processorNumber (s));
  return ((pointer)toThread - offsetofThread (s));
}
