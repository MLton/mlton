/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/*
 * All ML objects (including ML execution stacks) are allocated in a
 * contiguous heap.  The heap has the following general layout:
 * 
 * ---------------------------------------------------
 * |                                                 |
 * ---------------------------------------------------
 * ^
 * start
*/

typedef struct GC_heap {
  size_t numBytes; /* size of heap */
  pointer start; /* start of heap */
} *GC_heap;
