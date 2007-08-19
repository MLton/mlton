/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct GC_signalsInfo {
  /* TRUE iff a signal handler is running. */
  bool amInSignalHandler;   
  bool gcSignalHandled;
  bool gcSignalPending;
  /* TRUE iff a signal has been received but not handled by the
   * mutator.
   */
  volatile uint32_t signalIsPending; 
  /* The signals for which a mutator signal handler needs to run in
   * order to handle the signal.
   */
  sigset_t signalsHandled;
  /* The signals that have been recieved but not processed by the
   * mutator signal handler.
   */
  sigset_t signalsPending;
};

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initSignalStack (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
