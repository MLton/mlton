/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* isObjptr returns true if p looks like an object pointer. */
bool isObjptr (objptr p) {
  if GC_MODEL_NONOBJPTR {
    unsigned int shift = GC_MODEL_MINALIGN_SHIFT - GC_MODEL_SHIFT;
    objptr mask = ~((~((objptr)0)) << shift);
    return (0 == (p & mask));
  } else {
    return TRUE;
  }
}

