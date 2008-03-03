/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void numStackFramesAux (GC_state s, 
                        __attribute__ ((unused)) GC_frameIndex i) {
  s->callStackState.numStackFrames++;
}

uint32_t GC_numStackFrames (__attribute__ ((unused)) GC_state *gs) {
  GC_state s = pthread_getspecific (gcstate_key);
  s->callStackState.numStackFrames = 0;
  foreachStackFrame (s, numStackFramesAux);
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "%"PRIu32" = GC_numStackFrames [%d]\n", 
             s->callStackState.numStackFrames, Proc_processorNumber (s));
  return s->callStackState.numStackFrames;
}

void callStackAux (GC_state s, GC_frameIndex i) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "callStackAux ("FMTFI")\n", i);
  s->callStackState.callStack[s->callStackState.numStackFrames] = i;
  s->callStackState.numStackFrames++;
}

void GC_callStack (__attribute__ ((unused)) GC_state *gs, 
                   pointer p) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "GC_callStack [%d]\n", Proc_processorNumber (s));
  s->callStackState.numStackFrames = 0;
  s->callStackState.callStack = (uint32_t*)p;
  foreachStackFrame (s, callStackAux);
}

uint32_t* GC_frameIndexSourceSeq (__attribute__ ((unused)) GC_state *gs, 
                                  GC_frameIndex frameIndex) {
  uint32_t *res;
  GC_state s = pthread_getspecific (gcstate_key);
  res = s->sourceMaps.sourceSeqs[s->sourceMaps.frameSources[frameIndex]];
  if (DEBUG_CALL_STACK)
    fprintf (stderr, FMTPTR" = GC_frameIndexSourceSeq ("FMTFI") [%d]\n",
             (uintptr_t)res, frameIndex, Proc_processorNumber (s));
  return res;
}
