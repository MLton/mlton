/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

typedef struct GC_thread {
  /* The order of these fields is important.  The nonpointer fields
   * must be first, because this object must appear to be a normal
   * heap object.
   */
  size_t bytesNeeded; /* The number of bytes needed when returning
                       * to this thread.
                       */
  uint32_t exnStack;  /* An offset added to stackBottom that specifies 
                       * where the top of the exnStack is.
                       */
  objptr stack;       /* The stack for this thread. */
} *GC_thread;

#define BOGUS_EXN_STACK 0xFFFFFFFF

void displayThread (GC_state s, GC_thread thread, FILE *stream);
size_t sizeofThread (GC_state s);

