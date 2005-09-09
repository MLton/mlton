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
 *  ---------------------------------------------------
 *  |    old generation    |              |  nursery  |
 *  ---------------------------------------------------
 *  ^                                     ^
 *  start                                 nursery
*/

typedef struct GC_heap {
  pointer nursery; /* start of nursery */
  size_t oldGenBytes; /* size of old generation */
  pointer start; /* start of heap (and old generation) */
  size_t totalBytes; /* size of heap */
} *GC_heap;
