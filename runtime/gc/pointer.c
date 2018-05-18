/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* isPointer returns true if p looks like a pointer. */
bool isPointer (pointer p) {
  uintptr_t mask = ~((~((uintptr_t)0)) << GC_MODEL_MINALIGN_SHIFT);
  return (0 == ((uintptr_t)p & mask));
}
