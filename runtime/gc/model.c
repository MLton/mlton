/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline pointer objptrToPointer (objptr O, pointer B) {
  intptr_t O_ = (intptr_t)O;
  intptr_t B_;
  pointer P;

  if GC_MODEL_USEBASE {
    B_ = (intptr_t)B;
  } else {
    B_ = 0;
  }

  P = (pointer)((O_ << GC_MODEL_SHIFT) + B_);
  if (DEBUG_DETAILED) 
    fprintf (stderr, "objptrToPointer ("FMTOBJPTR") = "FMTPTR"\n", O, (intptr_t)P);
  
  return P;
}

static inline objptr pointerToObjptr (pointer P, pointer B) {
  intptr_t P_ = (intptr_t)P;
  intptr_t B_;
  objptr O;

  if GC_MODEL_USEBASE {
    B_ = (intptr_t)B;
  } else {
    B_ = 0;
  }

  O = (objptr)((P_ - B_) >> GC_MODEL_SHIFT);
  if (DEBUG_DETAILED) 
    fprintf (stderr, "pointerToObjptr ("FMTPTR") = "FMTOBJPTR"\n", (intptr_t)P, O);

  return O;
}
