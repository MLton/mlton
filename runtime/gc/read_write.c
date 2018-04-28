/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

static inline objptr readObjptr (FILE *f) {
  objptr res;
  fread_safe (&res, sizeof(objptr), 1, f);
  return res;
}

static inline pointer readPointer (FILE *f) {
  uintptr_t res;
  fread_safe (&res, sizeof(uintptr_t), 1, f);
  return (pointer)res;
}

static inline void writeObjptr (FILE *f, objptr op) {
  fwrite_safe (&op, sizeof(objptr), 1, f);
}

static inline void writePointer (FILE *f, pointer p) {
  uintptr_t u = (uintptr_t)p;
  fwrite_safe (&u, sizeof(uintptr_t), 1, f);
}
