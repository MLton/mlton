/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

bool detailedGCTime (GC_state s) {
  return s->controls.summary;
}

bool needGCTime (GC_state s) {
  return 
    DEBUG 
    or s->controls.summary 
    or s->controls.messages
    or s->controls.rusageMeasureGC;
}
