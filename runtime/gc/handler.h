/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void GC_startHandler (GC_state s);
void GC_finishHandler (GC_state s);
void switchToHandlerThreadIfNonAtomicAndSignalPending (GC_state s);
void GC_handler (GC_state s, int signum);
