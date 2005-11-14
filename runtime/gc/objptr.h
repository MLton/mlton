/* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define OBJPTR_TYPE__(z) uint ## z ## _t
#define OBJPTR_TYPE_(z) OBJPTR_TYPE__(z)
#define OBJPTR_TYPE OBJPTR_TYPE_(GC_MODEL_BITSIZE)
typedef OBJPTR_TYPE objptr;
#define OBJPTR_SIZE sizeof(objptr)
#define PRIxOBJPTR__(z) PRIx ## z
#define PRIxOBJPTR_(z) PRIxOBJPTR__(z)
#define PRIxOBJPTR PRIxOBJPTR_(GC_MODEL_BITSIZE)
#define FMTOBJPTR "0x%016"PRIxOBJPTR

#if GC_MODEL_NONOBJPTR
#define BOGUS_OBJPTR (objptr)0x1
#else
#error gc model does not admit bogus object pointer
#endif

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static bool isObjptr (objptr p);
static pointer objptrToPointer (objptr O, pointer B);
static objptr pointerToObjptr (pointer P, pointer B);
static pointer fetchObjptrToPointer (pointer OP, pointer B);
static void storeObjptrFromPointer (pointer OP, pointer P, pointer B);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */
