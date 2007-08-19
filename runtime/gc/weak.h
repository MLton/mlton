/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/*
 * Weak objects have the following layout:
 * 
 * header ::
 * padding ::
 * link (native-pointer) ::
 * objptr (object-pointer)
 *
 * The object type indexed by the header determines whether the weak
 * is valid or not.  If the type has numObjptrs == 1, then the weak
 * pointer is valid.  Otherwise, the type has numObjptrs == 0 and the
 * weak pointer is not valid.
 *
 * There may be zero or more bytes of padding for alignment purposes.
 *
 * The link native-pointer is used to chain the live weaks together
 * during a copying gc and is otherwise unused.
 *
 * The final component is the weak object-pointer.
 *
 * Note that the order of the fields is important.  The non-objptr
 * field must be first, because a weak object is sometimes treated as
 * a normal object.
 */ 
typedef struct GC_weak {
  struct GC_weak *link;
  objptr objptr;
} __attribute__ ((packed)) *GC_weak;

COMPILE_TIME_ASSERT(GC_weak__packed,
                    sizeof(struct GC_weak) ==
                    sizeof(struct GC_weak*)
                    + sizeof(objptr));

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline size_t sizeofWeak (GC_state s);
static inline size_t offsetofWeak (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

uint32_t GC_weakCanGet (GC_state s, pointer p);
pointer GC_weakGet (GC_state s, pointer p);
pointer GC_weakNew (GC_state s, GC_header header, pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
