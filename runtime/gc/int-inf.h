/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Layout of intInfs.  
 * Note, the value passed around is a pointer to the obj member.
 */
typedef struct GC_intInf {
  GC_arrayCounter counter;
  GC_arrayLength length;
  GC_header header;
  union {
    struct {
      mp_limb_t isneg;
      mp_limb_t limbs[1];
    } body;
    pointerAux _p; /* alignment */
  } obj;
} __attribute__ ((packed)) *GC_intInf;

COMPILE_TIME_ASSERT(GC_intInf__obj_packed,
                    offsetof(struct GC_intInf, obj) == 
                    sizeof(GC_arrayCounter) 
                    + sizeof(GC_arrayLength) 
                    + sizeof(GC_header));
COMPILE_TIME_ASSERT(GC_intInf__obj_body_isneg_packed,
                    offsetof(struct GC_intInf, obj.body.isneg) == 
                    offsetof(struct GC_intInf, obj));
COMPILE_TIME_ASSERT(GC_intInf__obj_body_limbs_packed,
                    offsetof(struct GC_intInf, obj.body.limbs) == 
                    offsetof(struct GC_intInf, obj) 
                    + sizeof(mp_limb_t));

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

COMPILE_TIME_ASSERT(sizeof_mp_limb_t__is_four_or_eight, 
                    (sizeof(mp_limb_t) == 4 || sizeof(mp_limb_t) == 8));
#define GC_INTINF_HEADER ( \
        CHAR_BIT * sizeof(mp_limb_t) == 32 ? \
        GC_WORD32_VECTOR_HEADER : ( \
        CHAR_BIT * sizeof(mp_limb_t) == 64 ? \
        GC_WORD64_VECTOR_HEADER : ( 0 ) ) )

COMPILE_TIME_ASSERT(sizeof_mp_limb_t__compat__sizeof_objptr, 
                    (sizeof(mp_limb_t) >= sizeof(objptr)) ||
                    (sizeof(objptr) % sizeof(mp_limb_t) == 0));
#define LIMBS_PER_OBJPTR ( \
        sizeof(mp_limb_t) >= sizeof(objptr) ? \
        1 : sizeof(objptr) / sizeof(mp_limb_t))

static inline void fillIntInfArg (GC_state s, objptr arg, __mpz_struct *res, 
                                  mp_limb_t space[LIMBS_PER_OBJPTR + 1]);
static inline void initIntInfRes (GC_state s, __mpz_struct *res, size_t bytes);
static inline objptr finiIntInfRes (GC_state s, __mpz_struct *res, size_t bytes);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#if (defined (MLTON_GC_INTERNAL_BASIS))

objptr IntInf_add (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_andb (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_gcd (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_mul (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_quot (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_orb (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_rem (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_sub (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_xorb (objptr lhs, objptr rhs, size_t bytes);
objptr IntInf_neg (objptr arg, size_t bytes);
objptr IntInf_notb (objptr arg, size_t bytes);
objptr IntInf_arshift (objptr arg, Word32_t shift, size_t bytes);
objptr IntInf_lshift (objptr arg, Word32_t shift, size_t bytes);
Int32_t IntInf_compare (objptr lhs, objptr rhs);
Bool_t IntInf_equal (objptr lhs, objptr rhs);
objptr IntInf_toString (objptr arg, Int32_t base, size_t bytes);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */
