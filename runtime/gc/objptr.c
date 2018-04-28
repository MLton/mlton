/* Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* isObjptr returns true if p looks like an object pointer. */
bool isObjptr (objptr p) {
  unsigned int shift = GC_MODEL_MINALIGN_SHIFT - GC_MODEL_OBJPTR_SHIFT;
  objptr mask = ~((~((objptr)0)) << shift);
  return (0 == (p & mask));
}

pointer objptrToPointer (objptr O, pointer B) {
  uintptr_t O_ = (uintptr_t)O;
  uintptr_t B_;
  unsigned int S_ = GC_MODEL_OBJPTR_SHIFT;
  uintptr_t P_;
  pointer P;

  if (GC_MODEL_OBJPTR_BASE) {
    B_ = (uintptr_t)B;
  } else {
    B_ = 0;
  }

  P_ = ((O_ << S_) + B_);
  P = (pointer)P_;
  if (DEBUG_OBJPTR) 
    fprintf (stderr, "objptrToPointer ("FMTOBJPTR") = "FMTPTR"\n", O, (uintptr_t)P);

  return P;
}

objptr pointerToObjptr (pointer P, pointer B) {
  uintptr_t P_ = (uintptr_t)P;
  uintptr_t B_;
  unsigned int S_ = GC_MODEL_OBJPTR_SHIFT;
  uintptr_t O_;
  objptr O;

  if (GC_MODEL_OBJPTR_BASE) {
    B_ = (uintptr_t)B;
  } else {
    B_ = 0;
  }

  O_ = ((P_ - B_) >> S_);
  O = (objptr)O_;
  if (DEBUG_OBJPTR) 
    fprintf (stderr, "pointerToObjptr ("FMTPTR") = "FMTOBJPTR"\n", (uintptr_t)P, O);

  return O;
}

/*
 * Note that by indirectly fetching and storing object pointers, the
 * following functions admit implementations that behave according to
 * model characteristics determined at runtime.  Hence, by making
 * exclusive use of these functions (and adding a GC_state->model
 * field set by the compiled program), we may be able to implement the
 * runtime in a manner which is agnostic to the actual objptr
 * representation.
 */
pointer fetchObjptrToPointer (pointer OP, pointer B) {
  return objptrToPointer (*((objptr*)OP), B);
}
void storeObjptrFromPointer (pointer OP, pointer P, pointer B) {
  *((objptr*)OP) = pointerToObjptr (P, B);
}
