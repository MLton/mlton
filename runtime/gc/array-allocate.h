/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

pointer GC_arrayAllocate (GC_state s, 
                          size_t ensureBytesFree, 
                          GC_arrayLength numElements, 
                          GC_header header);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
