/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

struct GC_callStackState {
  uint32_t numStackFrames;
  uint32_t *callStack;
};

uint32_t GC_numStackFrames (GC_state s);
void GC_callStack (GC_state s, pointer p);
uint32_t* GC_frameIndexSourceSeq (GC_state s, GC_frameIndex frameIndex);
