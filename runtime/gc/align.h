/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static size_t alignWithExtra (GC_state s, size_t bytes, size_t extra);
static bool isFrontierAligned (GC_state s, pointer p);
static pointer alignFrontier (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

pointer GC_alignFrontier (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
