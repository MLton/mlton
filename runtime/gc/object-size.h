/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

size_t sizeofArrayNoHeader (GC_state s, GC_arrayLength numElements, 
                            uint16_t numNonObjptrs, uint16_t numObjptrs);
size_t sizeofStackNoHeader (GC_state s, GC_stack stack);

size_t sizeofObject (GC_state s, pointer p);
