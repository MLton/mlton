/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline pointer newObject (GC_state s, GC_header header, 
                                 size_t bytesRequested, bool allocInOldGen);
static inline GC_stack newStack (GC_state s, size_t reserved, bool allocInOldGen);
static GC_thread newThread (GC_state s, size_t stackSize);

static inline void setFrontier (GC_state s, pointer p, size_t bytes);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
