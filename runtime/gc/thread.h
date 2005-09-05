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
   * Furthermore, the exnStack field must be first, because the native
   * codegen depends on this (which is bad and should be fixed).
   */
  uint32_t exnStack;     /* An offset added to stackBottom that specifies 
                          * where the top of the exnStack is.
                          */
  uint32_t bytesNeeded;  /* The number of bytes needed when returning
                          * to this thread.
                          */
  objptr stack;          /* The stack for this thread. */
} *GC_thread;

#define BOGUS_THREAD (GC_thread)BOGUS_POINTER
