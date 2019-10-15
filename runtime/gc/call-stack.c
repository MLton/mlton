/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void numStackFramesAux (__attribute__ ((unused)) GC_state s,
                        __attribute__ ((unused)) GC_frameIndex frameIndex,
                        __attribute__ ((unused)) GC_frameInfo frameInfo,
                        __attribute__ ((unused)) pointer frameTop,
                        uint32_t *numStackFrames) {
  *numStackFrames += 1;
}
void numStackFramesFun (GC_state s,
                        GC_frameIndex frameIndex,
                        GC_frameInfo frameInfo,
                        pointer frameTop,
                        void *env) {
  numStackFramesAux (s, frameIndex, frameInfo, frameTop, env);
}

uint32_t GC_numStackFrames (GC_state s) {
  uint32_t numStackFrames = 0;
  struct GC_foreachStackFrameClosure numStackFramesClosure =
    {.fun = numStackFramesFun, .env = &numStackFrames};
  foreachStackFrame (s, &numStackFramesClosure);
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "%"PRIu32" = GC_numStackFrames\n",
             numStackFrames);
  return numStackFrames;
}

void callStackAux (__attribute__ ((unused)) GC_state s,
                   GC_frameIndex frameIndex,
                   __attribute__ ((unused)) GC_frameInfo frameInfo,
                   __attribute__ ((unused)) pointer frameTop,
                   GC_callStackState callStackState) {
  callStackState->callStack[callStackState->numStackFrames] = frameIndex;
  callStackState->numStackFrames++;
}
void callStackFun (GC_state s,
                   GC_frameIndex frameIndex,
                   GC_frameInfo frameInfo,
                   pointer frameTop,
                   void *env) {
  callStackAux (s, frameIndex, frameInfo, frameTop, env);
}

void GC_callStack (GC_state s, pointer p) {
  if (DEBUG_CALL_STACK)
    fprintf (stderr, "GC_callStack\n");
  struct GC_callStackState callStackState = {.numStackFrames = 0, .callStack = (uint32_t*)p};
  struct GC_foreachStackFrameClosure callStackClosure =
    {.fun = callStackFun, .env = &callStackState};
  foreachStackFrame (s, &callStackClosure);
}

uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex) {
  uint32_t *res;

  res = s->sourceMaps.sourceSeqs[s->frameInfos[frameIndex].sourceSeqIndex];
  if (DEBUG_CALL_STACK)
    fprintf (stderr, FMTPTR" = GC_frameIndexSourceSeq ("FMTFI")\n",
             (uintptr_t)res, frameIndex);
  return res;
}
