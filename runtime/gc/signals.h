/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_signalsInfo {
  bool amInSignalHandler;   /* TRUE iff a signal handler is running. */
  bool gcSignalHandled;
  bool gcSignalPending;
  volatile bool signalIsPending; /* TRUE iff a signal has been received 
                                  * but not handled by the mutator.
                                  */
  /* signalsHandled is the set of signals for which a mutator signal
   * handler needs to run in order to handle the signal.
   */
  sigset_t signalsHandled;
  /* The signals that have been recieved but not processed by the mutator
   * signal handler.
   */
  sigset_t signalsPending;
};
