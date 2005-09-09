/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline pointer objptrToPointer (objptr O, pointer B) {
  uintptr_t O_ = (uintptr_t)O;
  uintptr_t B_;
  unsigned int S_ = GC_MODEL_SHIFT;
  uintptr_t P_;
  pointer P;

  if GC_MODEL_USEBASE {
    B_ = (uintptr_t)B;
  } else {
    B_ = 0;
  }

  P_ = ((O_ << S_) + B_);
  P = (pointer)P_;
  if (DEBUG_DETAILED) 
    fprintf (stderr, "objptrToPointer ("FMTOBJPTR") = "FMTPTR"\n", O, (uintptr_t)P);
  
  return P;
}

static inline objptr pointerToObjptr (pointer P, pointer B) {
  uintptr_t P_ = (uintptr_t)P;
  uintptr_t B_;
  unsigned int S_ = GC_MODEL_SHIFT;
  uintptr_t O_;
  objptr O;

  if GC_MODEL_USEBASE {
    B_ = (uintptr_t)B;
  } else {
    B_ = 0;
  }

  O_ = ((P_ - B_) >> S_);
  O = (objptr)O_;
  if (DEBUG_DETAILED) 
    fprintf (stderr, "pointerToObjptr ("FMTPTR") = "FMTOBJPTR"\n", (uintptr_t)P, O);

  return O;
}

/* isObjptr returns true if p looks like an object pointer. */
static inline bool isObjptr (objptr p) {
  if GC_MODEL_NONPTR {
    unsigned int shift = GC_MODEL_MINALIGN_SHIFT - GC_MODEL_SHIFT;
    objptr mask = ~((~((objptr)0)) << shift);
    return (0 == (p & mask));
  } else {
    return TRUE;
  }
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
static inline pointer fetchObjptrToPointer (pointer OP, pointer B) {
  return objptrToPointer (*((objptr*)OP), B);
}
static inline void storeObjptrFromPointer (pointer OP, pointer P, pointer B) {
  *((objptr*)OP) = pointerToObjptr (P, B);
}
static inline size_t objptrSize (void) {
  return OBJPTR_SIZE;
}
