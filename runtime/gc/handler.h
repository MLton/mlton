/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void switchToSignalHandlerThreadIfNonAtomicAndSignalPending (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_startSignalHandler (GC_state s);
void GC_finishSignalHandler (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

void GC_handler (GC_state s, int signum);
