/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define BOGUS_EXN_STACK 0xFFFFFFFF

void displayThread (GC_state s,
                    GC_thread thread, 
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIu32"\n"
          "\t\tbytesNeeded = %zu\n"
          "\t\tstack = "FMTOBJPTR"\n",
          thread->exnStack,
          thread->bytesNeeded,
          thread->stack);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, s->heap.start)),
                stream);
}

static inline size_t threadSize (GC_state s) {
  size_t res;

  res = GC_NORMAL_HEADER_SIZE + sizeof (struct GC_thread);
  /* The following assert depends on struct GC_thread being the right
   * size.  Right now, it happens that res = 16, which is aligned mod
   * 4 and mod 8, which is all that we need.  If the struct every
   * changes (possible) or we need more alignment (doubtful), we may
   * need to put some padding at the beginning.
   */
  assert (isAligned (res, s->alignment));
  return res;
}
