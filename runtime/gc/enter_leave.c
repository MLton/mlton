/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* enter and leave should be called at the start and end of every GC
 * function that is exported to the outside world.  They make sure
 * that the function is run in a critical section and check the GC
 * invariant.
 */
static void enter (GC_state s) {

  if (DEBUG)
    fprintf (stderr, "enter\n");
  /* used needs to be set because the mutator has changed s->stackTop. */
  currentThreadStack(s)->used = currentStackUsed (s);
  currentThread(s)->exnStack = s->exnStack;
  if (DEBUG) 
    displayGCState (s, stderr);
  atomicBegin (s);
  assert (invariant (s));
  if (DEBUG)
    fprintf (stderr, "enter ok\n");
}

static void leave (GC_state s) {
  if (DEBUG)
    fprintf (stderr, "leave\n");
  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  assert (mutatorInvariant (s, FALSE, TRUE));
  atomicEnd (s);
  if (DEBUG)
    fprintf (stderr, "leave ok\n");
}
