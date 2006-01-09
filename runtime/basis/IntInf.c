/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"
typedef unsigned int uint;

enum {
  DEBUG_INT_INF = FALSE,
};

/* Import the global gcState so we can get and set the frontier. */
extern struct GC_state gcState;

/*
 * Test if a intInf is a fixnum.
 */
static inline bool isSmall (pointer arg) {
        return ((uintptr_t)arg & 1);
}

static inline bool eitherIsSmall (pointer arg1, pointer arg2) {
        return (((uintptr_t)arg1 | (uintptr_t)arg2) & 1);
}

static inline bool areSmall (pointer arg1, pointer arg2) {
        return ((uintptr_t)arg1 & (uintptr_t)arg2 & 1);
}

/*
 * Convert a bignum intInf to a bignum pointer.
 */
static inline GC_intInf toBignum (pointer arg) {
        GC_intInf bp;

        assert(not isSmall(arg));
        bp = (GC_intInf)(arg - offsetof(struct GC_intInf, isneg));
        if (DEBUG_INT_INF)
                fprintf (stderr, "bp->header = "FMTHDR"\n", bp->header);
        assert (bp->header == GC_intInfHeader ());
        return bp;
}

/*
 * Given an intInf, a pointer to an __mpz_struct and something large enough
 * to contain 2 limbs, fill in the __mpz_struct.
 */
static inline void fill (pointer arg, __mpz_struct *res, mp_limb_t space[2]) {
        GC_intInf bp;

        if (DEBUG_INT_INF)
                fprintf (stderr, "fill ("FMTPTR", "FMTPTR", "FMTPTR")\n",
                                (uintptr_t)arg, (uintptr_t)res, (uintptr_t)space);
        if (isSmall(arg)) {
                res->_mp_alloc = 2;
                res->_mp_d = space;
                if ((int)arg > 1) {
                        res->_mp_size = 1;
                        space[0] = (uint)arg >> 1;
                } else if ((int)arg < 0) {
                        res->_mp_size = -1;
                        space[0] = - (int)((uint)arg>>1 | (uint)1<<31);
                } else
                        res->_mp_size = 0;
        } else {
                bp = toBignum(arg);
                res->_mp_alloc = bp->length - 1;
                res->_mp_d = (mp_limb_t*)(bp->limbs);
                res->_mp_size = bp->isneg ? - res->_mp_alloc
                                        : res->_mp_alloc;
        }
}

/*
 * Initialize an __mpz_struct to use the space provided by an ML array.
 */
static inline void initRes (__mpz_struct *mpzp, size_t bytes) {
        GC_intInf bp;

        assert (bytes <= (size_t)(gcState.limitPlusSlop - gcState.frontier));
        bp = (GC_intInf)gcState.frontier;
        /* We have as much space for the limbs as there is to the end
         * of the heap.  Divide by (sizeof(mp_limb_t)) to get number
         * of limbs.
         */
        mpzp->_mp_alloc = (gcState.limitPlusSlop - (pointer)bp->limbs) / (sizeof(mp_limb_t));
        mpzp->_mp_size = 0; /* is this necessary? */
        mpzp->_mp_d = (mp_limb_t*)(bp->limbs);
}

/*
 * Count number of leading zeros.  The argument will not be zero.
 * This MUST be replaced with assembler.
 */
static inline uint leadingZeros (mp_limb_t word) {
        uint    res;

        assert(word != 0);
        res = 0;
        while ((int)word > 0) {
                ++res;
                word <<= 1;
        }
        return (res);
}

static inline void setFrontier (pointer p, size_t bytes) {
        p = GC_alignFrontier (&gcState, p);
        assert ((size_t)(p - gcState.frontier) <= bytes);
        GC_profileAllocInc (&gcState, p - gcState.frontier);
        gcState.frontier = p;
        assert (gcState.frontier <= gcState.limitPlusSlop);
}

/*
 * Given an __mpz_struct pointer which reflects the answer, set gcState.frontier
 * and return the answer.
 * If the answer fits in a fixnum, we return that, with the frontier
 * rolled back.
 * If the answer doesn't need all of the space allocated, we adjust
 * the array size and roll the frontier slightly back.
 */
static pointer answer (__mpz_struct *ans, size_t bytes) {
        GC_intInf               bp;
        int                     size;

        bp = (GC_intInf)((pointer)ans->_mp_d - offsetof(struct GC_intInf, limbs));
        assert(ans->_mp_d == (mp_limb_t*)(bp->limbs));
        size = ans->_mp_size;
        if (size < 0) {
                bp->isneg = TRUE;
                size = - size;
        } else
                bp->isneg = FALSE;
        if (size <= 1) {
                uint    val,
                        ans;

                if (size == 0)
                        val = 0;
                else
                        val = bp->limbs[0];
                if (bp->isneg) {
                        /*
                         * We only fit if val in [1, 2^30].
                         */
                        ans = - val;
                        val = val - 1;
                } else
                        /*
                         * We only fit if val in [0, 2^30 - 1].
                         */
                        ans = val;
                if (val < (uint)1<<30) {
                        return (pointer)(ans<<1 | 1);
                }
        }
        setFrontier ((pointer)(&bp->limbs[size]), bytes);
        bp->counter = 0;
        bp->length = size + 1; /* +1 for isNeg word */
        bp->header = GC_intInfHeader ();
        return (pointer)&bp->isneg;
}

static inline pointer binary (pointer lhs, pointer rhs, size_t bytes,
                                void(*binop)(__mpz_struct *resmpz, 
                                        __gmp_const __mpz_struct *lhsspace,
                                        __gmp_const __mpz_struct *rhsspace)) {
        __mpz_struct    lhsmpz,
                        rhsmpz,
                        resmpz;
        mp_limb_t       lhsspace[2],
                        rhsspace[2];

        initRes (&resmpz, bytes);
        fill (lhs, &lhsmpz, lhsspace);
        fill (rhs, &rhsmpz, rhsspace);
        binop (&resmpz, &lhsmpz, &rhsmpz);
        return answer (&resmpz, bytes);
}

pointer IntInf_add (pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_add ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_add);
}

pointer IntInf_gcd (pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_gcd ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_gcd);
}

pointer IntInf_mul (pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_mul ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_mul);
}

pointer IntInf_sub (pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_sub ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_sub);
}

pointer IntInf_andb(pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_andb ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_and);
}

pointer IntInf_orb(pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_orb ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_ior);
}

pointer IntInf_xorb(pointer lhs, pointer rhs, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_xorb ("FMTPTR", "FMTPTR", %zu)\n",
                                (uintptr_t)lhs, (uintptr_t)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_xor);
}

static pointer
unary(pointer arg, size_t bytes,
      void(*unop)(__mpz_struct *resmpz, 
                  __gmp_const __mpz_struct *argspace))
{
        __mpz_struct    argmpz,
                        resmpz;
        mp_limb_t       argspace[2];

        initRes(&resmpz, bytes);
        fill(arg, &argmpz, argspace);
        unop(&resmpz, &argmpz);
        return answer (&resmpz, bytes);
}

pointer IntInf_neg(pointer arg, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_neg ("FMTPTR", %zu)\n",
                                (uintptr_t)arg, bytes);
        return unary(arg, bytes, &mpz_neg);
}

pointer IntInf_notb(pointer arg, size_t bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_notb ("FMTPTR", %zu)\n",
                                (uintptr_t)arg, bytes);
        return unary(arg, bytes, &mpz_com);
}

static pointer
shary(pointer arg, uint shift, size_t bytes,
      void(*shop)(__mpz_struct *resmpz, 
                  __gmp_const __mpz_struct *argspace,
                  unsigned long shift))
{
        __mpz_struct    argmpz,
                        resmpz;
        mp_limb_t       argspace[2];

        initRes(&resmpz, bytes);
        fill(arg, &argmpz, argspace);
        shop(&resmpz, &argmpz, (unsigned long)shift);
        return answer (&resmpz, bytes);
}

pointer IntInf_arshift(pointer arg, Word shift_w, size_t bytes) {
        uint shift = (uint)shift_w;
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_arshift ("FMTPTR", %u, %zu)\n",
                                (uintptr_t)arg, shift, bytes);
        return shary(arg, shift, bytes, &mpz_fdiv_q_2exp);
}

pointer IntInf_lshift(pointer arg, Word shift_w, size_t bytes) {
        uint shift = (uint)shift_w;
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_lshift ("FMTPTR", %u, %zu)\n",
                                (uintptr_t)arg, shift, bytes);
        return shary(arg, shift, bytes, &mpz_mul_2exp);
}

Word
IntInf_smallMul(Word lhs, Word rhs, pointer carry)
{
        intmax_t   prod;

        prod = (intmax_t)(int)lhs * (int)rhs;
        *(uint *)carry = (uintmax_t)prod >> 32;
        return ((uint)(uintmax_t)prod);
}

/*
 * Return an integer which compares to 0 as the two intInf args compare
 * to each other.
 */
Int IntInf_compare (pointer lhs, pointer rhs) {
        __mpz_struct            lhsmpz,
                                rhsmpz;
        mp_limb_t               lhsspace[2],
                                rhsspace[2];

        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_compare ("FMTPTR", "FMTPTR")\n",
                                (uintptr_t)lhs, (uintptr_t)rhs);
        fill (lhs, &lhsmpz, lhsspace);
        fill (rhs, &rhsmpz, rhsspace);
        return mpz_cmp (&lhsmpz, &rhsmpz);
}

/*
 * Check if two IntInf.int's are equal.
 */
Bool IntInf_equal (pointer lhs, pointer rhs) {
        if (lhs == rhs)
                return TRUE;
        if (eitherIsSmall (lhs, rhs))
                return FALSE;
        else
                return 0 == IntInf_compare (lhs, rhs);
}

/*
 * Convert an intInf to a string.
 * Arg is an intInf, base is the base to use (2, 8, 10 or 16) and space is a
 * string (mutable) which is large enough.
 */
pointer IntInf_toString (pointer arg, int base, size_t bytes) {
        GC_string       sp;
        __mpz_struct    argmpz;
        mp_limb_t       argspace[2];
        char            *str;
        uint            size;
        uint            i;
        char            c;

        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_toString ("FMTPTR", %d, %zu)\n",
                                (uintptr_t)arg, base, bytes);
        assert (base == 2 || base == 8 || base == 10 || base == 16);
        fill (arg, &argmpz, argspace);
        sp = (GC_string)gcState.frontier;
        str = mpz_get_str(sp->chars, base, &argmpz);
        assert(str == sp->chars);
        size = strlen(str);
        if (*sp->chars == '-')
                *sp->chars = '~';
        if (base > 0)
                for (i = 0; i < size; i++) {
                        c = sp->chars[i];
                        if (('a' <= c) && (c <= 'z'))
                                sp->chars[i] = c + ('A' - 'a');
                }
        sp->counter = 0;
        sp->length = size;
        sp->header = GC_stringHeader ();
        setFrontier ((pointer)(&sp->chars[align(size, 4)]), bytes);
        return (pointer)str;
}

/*
 * Quotient (round towards 0, remainder is returned by IntInf_rem).
 * space is a word array with enough space for the quotient
 *      num limbs + 1 - den limbs
 * shifted numerator
 *      num limbs + 1
 * and shifted denominator
 *      den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer IntInf_quot (pointer num, pointer den, size_t bytes) {
        __mpz_struct    resmpz,
                        nmpz,
                        dmpz;
        mp_limb_t       nss[2],
                        dss[2],
                        carry,
                        *np,
                        *dp;
        int             nsize,
                        dsize,
                        qsize;
        bool            resIsNeg;
        uint            shift;

        initRes(&resmpz, bytes);
        fill(num, &nmpz, nss);
        resIsNeg = FALSE;
        nsize = nmpz._mp_size;
        if (nsize < 0) {
                nsize = - nsize;
                resIsNeg = TRUE;
        }
        fill(den, &dmpz, dss);
        dsize = dmpz._mp_size;
        if (dsize < 0) {
                dsize = - dsize;
                resIsNeg = not resIsNeg;
        }
        assert(dsize != 0 && dmpz._mp_d[dsize - 1] != 0);
        assert((nsize == 0 && dsize == 1)
        or (nsize >= dsize && nmpz._mp_d[nsize - 1] != 0));
        qsize = 1 + nsize - dsize;
        if (dsize == 1) {
                if (nsize == 0)
                        return (pointer)1; /* tagged 0 */
                mpn_divrem_1(resmpz._mp_d,
                             (mp_size_t)0,
                             nmpz._mp_d,
                             nsize,
                             dmpz._mp_d[0]);
                if (resmpz._mp_d[qsize - 1] == 0)
                        --qsize;
        } else {
                np = &resmpz._mp_d[qsize];
                shift = leadingZeros(dmpz._mp_d[dsize - 1]);
                if (shift == 0) {
                        dp = dmpz._mp_d;
                        memcpy((void *)np,
                               nmpz._mp_d,
                               nsize * sizeof(*nmpz._mp_d));
                } else {
                        carry = mpn_lshift(np, nmpz._mp_d, nsize, shift);
                        unless (carry == 0)
                                np[nsize++] = carry;
                        dp = &np[nsize];
                        mpn_lshift(dp, dmpz._mp_d, dsize, shift);
                }
                carry = mpn_divrem(resmpz._mp_d,
                                   (mp_size_t)0,
                                   np,
                                   nsize,
                                   dp,
                                   dsize);
                qsize = nsize - dsize;
                if (carry != 0)
                        resmpz._mp_d[qsize++] = carry;
        }
        resmpz._mp_size = resIsNeg ? - qsize : qsize;
        return answer (&resmpz, bytes);
}


/*
 * Remainder (sign taken from numerator, quotient is returned by IntInf_quot).
 * space is a word array with enough space for the remainder
 *      den limbs
 * shifted numerator
 *      num limbs + 1
 * and shifted denominator
 *      den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer IntInf_rem (pointer num, pointer den, size_t bytes) {
        __mpz_struct    resmpz,
                        nmpz,
                        dmpz;
        mp_limb_t       nss[2],
                        dss[2],
                        carry,
                        *dp;
        int             nsize,
                        dsize;
        bool            resIsNeg;
        uint            shift;

        initRes(&resmpz, bytes);
        fill(num, &nmpz, nss);
        nsize = nmpz._mp_size;
        resIsNeg = nsize < 0;
        if (resIsNeg)
                nsize = - nsize;
        fill(den, &dmpz, dss);
        dsize = dmpz._mp_size;
        if (dsize < 0)
                dsize = - dsize;
        assert(dsize != 0 && dmpz._mp_d[dsize - 1] != 0);
        assert((nsize == 0 && dsize == 1)
        or (nsize >= dsize && nmpz._mp_d[nsize - 1] != 0));
        if (dsize == 1) {
                if (nsize == 0)
                        resmpz._mp_size = 0;
                else {
                        carry = mpn_mod_1(nmpz._mp_d, nsize, dmpz._mp_d[0]);
                        if (carry == 0)
                                nsize = 0;
                        else {
                                resmpz._mp_d[0] = carry;
                                nsize = 1;
                        }
                }
        } else {
                shift = leadingZeros(dmpz._mp_d[dsize - 1]);
                if (shift == 0) {
                        dp = dmpz._mp_d;
                        memcpy((void *)resmpz._mp_d,
                               (void *)nmpz._mp_d,
                               nsize * sizeof(*nmpz._mp_d));
                } else {
                        carry = mpn_lshift(resmpz._mp_d,
                                           nmpz._mp_d,
                                           nsize,
                                           shift);
                        unless (carry == 0)
                                resmpz._mp_d[nsize++] = carry;
                        dp = &resmpz._mp_d[nsize];
                        mpn_lshift(dp, dmpz._mp_d, dsize, shift);
                }
                mpn_divrem(&resmpz._mp_d[dsize],
                           (mp_size_t)0,
                           resmpz._mp_d,
                           nsize,
                           dp,
                           dsize);
                nsize = dsize;
                assert(nsize > 0);
                while (resmpz._mp_d[nsize - 1] == 0)
                        if (--nsize == 0)
                                break;
                unless (nsize == 0 || shift == 0) {
                        mpn_rshift(resmpz._mp_d, resmpz._mp_d, nsize, shift);
                        if (resmpz._mp_d[nsize - 1] == 0)
                                --nsize;
                }
        }
        resmpz._mp_size = resIsNeg ? - nsize : nsize;
        return answer (&resmpz, bytes);
}
