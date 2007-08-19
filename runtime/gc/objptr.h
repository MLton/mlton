/* Copyright (C) 2005-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define OBJPTR_TYPE__(z) uint ## z ## _t
#define OBJPTR_TYPE_(z) OBJPTR_TYPE__(z)
#define OBJPTR_TYPE OBJPTR_TYPE_(GC_MODEL_OBJPTR_SIZE)
typedef OBJPTR_TYPE objptr;
#define OBJPTR_SIZE sizeof(objptr)
#define PRIxOBJPTR__(z) PRIx ## z
#define PRIxOBJPTR_(z) PRIxOBJPTR__(z)
#define PRIxOBJPTR PRIxOBJPTR_(GC_MODEL_OBJPTR_SIZE)
#define FMTOBJPTR "0x%016"PRIxOBJPTR

COMPILE_TIME_ASSERT(sizeof_voidStar__gte__sizeof_objptr,
                    sizeof(void*) >= sizeof(objptr));

#define BOGUS_OBJPTR (objptr)0x1

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isObjptr (objptr p);
static inline pointer objptrToPointer (objptr O, pointer B);
static inline objptr pointerToObjptr (pointer P, pointer B);
static inline pointer fetchObjptrToPointer (pointer OP, pointer B);
static inline void storeObjptrFromPointer (pointer OP, pointer P, pointer B);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
