/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void displayGCState (GC_state s, FILE *stream) {
  fprintf (stream,
           "GC state\n");
  fprintf (stream, "\tcurrentThread"FMTOBJPTR"\n", s->currentThread);
  displayThread (s, (GC_thread)(objptrToPointer (s->currentThread, s->heap.start)), 
                 stream);
  fprintf (stream, "\tgenerational\n");
  displayGenerationalMaps (s, &s->generationalMaps, 
                           stream);
  fprintf (stream, "\theap\n");
  displayHeap (s, &s->heap, 
               stream);
  fprintf (stream,
           "\tlimit = "FMTPTR"\n"
           "\tstackBottom = "FMTPTR"\n"
           "\tstackTop = "FMTPTR"\n",
           (uintptr_t)s->limit,
           (uintptr_t)s->stackBottom,
           (uintptr_t)s->stackTop);
}
