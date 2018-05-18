/* Copyright (C) 2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline size_t sizeofArrayNoMetaData (GC_state s, GC_arrayLength numElements,
                                            uint16_t bytesNonObjptrs, uint16_t numObjptrs);
static inline size_t sizeofStackNoMetaData (GC_state s, GC_stack stack);

static inline void sizeofObjectAux (GC_state s, pointer p,
                                    size_t* metaDataBytes, size_t* objectBytes);

static inline size_t sizeofObject (GC_state s, pointer p);

static inline size_t sizeofObjectNoMetaData (GC_state s, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
