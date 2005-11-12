#include "platform.h"

extern struct GC_state gcState;

enum {
  DEBUG_THREAD = FALSE,
};

Pointer Thread_current () {
  Pointer t;
  
  t = (Pointer)(GC_getCurrentThread (&gcState));
  if (DEBUG_THREAD)
    fprintf (stderr, FMTPTR" = Thread_current ()\n", 
             (uintptr_t)t);
  return t;
}

void Thread_finishSignalHandler () {
  GC_finishSignalHandler (&gcState);
}

Pointer Thread_saved () {
  Pointer t;
  
  t = (Pointer)(GC_getSavedThread (&gcState));
  if (DEBUG_THREAD)
    fprintf (stderr, FMTPTR" = Thread_saved ()\n", 
             (uintptr_t)t);
  return t;
}

void Thread_setCallFromCHandler (Pointer t) {
  GC_setCallFromCHandlerThread (&gcState, (GC_thread)t);
}

void Thread_setSaved (Pointer t) {
  if (DEBUG_THREAD)
    fprintf (stderr, "Thread_setSaved ("FMTPTR")\n", 
             (uintptr_t)t);
  GC_setSavedThread (&gcState, (GC_thread)t);
}

void Thread_setSignalHandler (Pointer t) {
  GC_setSignalHandlerThread (&gcState, (GC_thread)t);
}

void Thread_startSignalHandler () {
  GC_startSignalHandler (&gcState);
}

void Thread_switchTo (Pointer t, Word ensureBytesFree) {
  if (DEBUG_THREAD)
    fprintf (stderr, "Thread_switchTo ("FMTPTR", %u)\n",
             (uintptr_t)t, (uint)ensureBytesFree);
  GC_switchToThread (&gcState, (GC_thread)t, ensureBytesFree);
}
