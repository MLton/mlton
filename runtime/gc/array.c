/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* getArrayCounterp (p)
 *
 * Returns a pointer to the counter for the array pointed to by p.
 */
static inline GC_arrayCounter* getArrayCounterp (pointer a) {
  return (GC_arrayCounter*)(a - GC_HEADER_SIZE 
                            - GC_ARRAY_LENGTH_SIZE - GC_ARRAY_COUNTER_SIZE);
}

/* getArrayCounter (p)
 *
 * Returns the counter for the array pointed to by p.
 */
static inline GC_arrayCounter getArrayCounter (pointer a) {
  return *(getArrayCounterp (a));
}
