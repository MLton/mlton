/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Layout of intInfs.  
 * Note, the value passed around is a pointer to the isneg member.
 */
typedef struct GC_intInf {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  mp_limb_t isneg;
  mp_limb_t limbs[1];
} *GC_intInf;

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

COMPILE_TIME_ASSERT(sizeof_mp_limb_t__is_four_or_eight, 
                    (sizeof(mp_limb_t) == 4 || sizeof(mp_limb_t) == 8));
#define GC_INTINF_HEADER ( \
        CHAR_BIT * sizeof(mp_limb_t) == 32 ? \
        GC_WORD32_VECTOR_HEADER : ( \
        CHAR_BIT * sizeof(mp_limb_t) == 64 ? \
        GC_WORD64_VECTOR_HEADER : ( 0 ) ) )

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

GC_header GC_intInfHeader (void);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
