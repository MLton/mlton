/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void numStackFramesAux (GC_state s, 
                        __attribute__ ((unused)) GC_frameIndex i) {
  s->callStackState.numStackFrames++;
}

uint32_t GC_numStackFrames (GC_state s) {
  s->callStackState.numStackFrames = 0;
  foreachStackFrame (s, numStackFramesAux);
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "%"PRIu32" = GC_numStackFrames\n", 
             s->callStackState.numStackFrames);
  return s->callStackState.numStackFrames;
}

void callStackAux (GC_state s, GC_frameIndex i) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "callStackAux ("FMTFI")\n", i);
  s->callStackState.callStack[s->callStackState.numStackFrames] = i;
  s->callStackState.numStackFrames++;
}

void GC_callStack (GC_state s, pointer p) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "GC_callStack\n");
  s->callStackState.numStackFrames = 0;
  s->callStackState.callStack = (uint32_t*)p;
  foreachStackFrame (s, callStackAux);
}

uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex) {
  uint32_t *res;

  res = s->sourceMaps.sourceSeqs[s->sourceMaps.frameSources[frameIndex]];
  if (DEBUG_CALL_STACK)
    fprintf (stderr, FMTPTR" = GC_frameIndexSourceSeq ("FMTFI")\n",
             (uintptr_t)res, frameIndex);
  return res;
}
