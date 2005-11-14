/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void switchToThread (GC_state s, objptr op);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

void GC_switchToThread (GC_state s, GC_thread t, size_t ensureBytesFree);
