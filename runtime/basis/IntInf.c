/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "platform.h"

enum {
        DEBUG_INT_INF = FALSE,
};

/* Import the global gcState so we can get and set the frontier. */
extern struct GC_state gcState;

/*
 * Layout of strings.  Note, the value passed around is a pointer to
 * the chars member.
 */
typedef struct        strng {
        uint        counter,        /* used by GC. */
                card,                /* number of chars */
                magic;                /* STRMAGIC */
        char        chars[0];        /* actual chars */
}        strng;

/*
 * Test if a intInf is a fixnum.
 */
static inline uint isSmall (pointer arg) {
        return ((uint)arg & 1);
}

static inline uint eitherIsSmall (pointer arg1, pointer arg2) {
        return (1 & ((uint)arg1 | (uint)arg2));
}

static inline uint areSmall (pointer arg1, pointer arg2) {
        return ((uint)arg1 & (uint)arg2 & 1);
}

/*
 * Convert a bignum intInf to a bignum pointer.
 */
static inline bignum * toBignum (pointer arg) {
        bignum        *bp;

        assert(not isSmall(arg));
        bp = (bignum *)((uint)arg - offsetof(struct bignum, isneg));
        if (DEBUG_INT_INF)
                fprintf (stderr, "bp->magic = 0x%08x\n", bp->magic);
        assert (bp->magic == BIGMAGIC);
        return bp;
}

/*
 * Given an intInf, a pointer to an __mpz_struct and something large enough
 * to contain 2 limbs, fill in the __mpz_struct.
 */
static inline void fill (pointer arg, __mpz_struct *res, mp_limb_t space[2]) {
        bignum        *bp;

        if (DEBUG_INT_INF)
                fprintf (stderr, "fill (0x%08x, 0x%08x, 0x%08x)\n",
                                (uint)arg, (uint)res, (uint)space);
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
                res->_mp_alloc = bp->card - 1;
                res->_mp_d = bp->limbs;
                res->_mp_size = bp->isneg ? - res->_mp_alloc
                                        : res->_mp_alloc;
        }
}

/*
 * Initialize an __mpz_struct to use the space provided by an ML array.
 */
static inline void initRes (__mpz_struct *mpzp, uint bytes) {
        struct bignum *bp;

        assert (bytes <= gcState.limitPlusSlop - gcState.frontier);
        bp = (bignum*)gcState.frontier;
        /* We have as much space for the limbs as there is to the end of the 
         * heap.  Divide by 4 to get number of words. 
         */
        mpzp->_mp_alloc = (gcState.limitPlusSlop - (pointer)bp->limbs) / 4;
        mpzp->_mp_size = 0; /* is this necessary? */
        mpzp->_mp_d = bp->limbs;
}

/*
 * Count number of leading zeros.  The argument will not be zero.
 * This MUST be replaced with assembler.
 */
static inline uint leadingZeros (mp_limb_t word) {
        uint        res;

        assert(word != 0);
        res = 0;
        while ((int)word > 0) {
                ++res;
                word <<= 1;
        }
        return (res);
}

static inline void setFrontier (pointer p, uint bytes) {
        p = GC_alignFrontier (&gcState, p);
        assert (p - gcState.frontier <= bytes);
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
static pointer answer (__mpz_struct *ans, uint bytes) {
        bignum                        *bp;
        int                        size;

        bp = (bignum *)((pointer)ans->_mp_d - offsetof(struct bignum, limbs));
        assert(ans->_mp_d == bp->limbs);
        size = ans->_mp_size;
        if (size < 0) {
                bp->isneg = TRUE;
                size = - size;
        } else
                bp->isneg = FALSE;
        if (size <= 1) {
                uint        val,
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
        setFrontier ((pointer)&bp->limbs[size], bytes);
        bp->counter = 0;
        bp->card = size + 1; /* +1 for isNeg word */
        bp->magic = BIGMAGIC;
        return (pointer)&bp->isneg;
}

static inline pointer binary (pointer lhs, pointer rhs, uint bytes,
                                void(*binop)(__mpz_struct *resmpz, 
                                        __gmp_const __mpz_struct *lhsspace,
                                        __gmp_const __mpz_struct *rhsspace)) {
        __mpz_struct        lhsmpz,
                        rhsmpz,
                        resmpz;
        mp_limb_t        lhsspace[2],
                        rhsspace[2];

        initRes (&resmpz, bytes);
        fill (lhs, &lhsmpz, lhsspace);
        fill (rhs, &rhsmpz, rhsspace);
        binop (&resmpz, &lhsmpz, &rhsmpz);
        return answer (&resmpz, bytes);
}

pointer IntInf_add (pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_add (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_add);
}

pointer IntInf_gcd (pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_gcd (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_gcd);
}

pointer IntInf_mul (pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_mul (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_mul);
}

pointer IntInf_sub (pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_sub (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary (lhs, rhs, bytes, &mpz_sub);
}

pointer IntInf_andb(pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_andb (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_and);
}

pointer IntInf_orb(pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_orb (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_ior);
}

pointer IntInf_xorb(pointer lhs, pointer rhs, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_xorb (0x%08x, 0x%08x, %u)\n",
                                (uint)lhs, (uint)rhs, bytes);
        return binary(lhs, rhs, bytes, &mpz_xor);
}

static pointer
unary(pointer arg, uint bytes,
      void(*unop)(__mpz_struct *resmpz, 
                  __gmp_const __mpz_struct *argspace))
{
        __mpz_struct        argmpz,
                        resmpz;
        mp_limb_t        argspace[2];

        initRes(&resmpz, bytes);
        fill(arg, &argmpz, argspace);
        unop(&resmpz, &argmpz);
        return answer (&resmpz, bytes);
}

pointer IntInf_neg(pointer arg, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_neg (0x%08x, %u)\n",
                                (uint)arg, bytes);
        return unary(arg, bytes, &mpz_neg);
}

pointer IntInf_notb(pointer arg, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_notb (0x%08x, %u)\n",
                                (uint)arg, bytes);
        return unary(arg, bytes, &mpz_com);
}

static pointer
shary(pointer arg, uint shift, uint bytes,
      void(*shop)(__mpz_struct *resmpz, 
                  __gmp_const __mpz_struct *argspace,
                  ulong shift))
{
        __mpz_struct        argmpz,
                        resmpz;
        mp_limb_t        argspace[2];

        initRes(&resmpz, bytes);
        fill(arg, &argmpz, argspace);
        shop(&resmpz, &argmpz, (ulong)shift);
        return answer (&resmpz, bytes);
}

pointer IntInf_arshift(pointer arg, uint shift, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_arshift (0x%08x, %u, %u)\n",
                                (uint)arg, shift, bytes);
        return shary(arg, shift, bytes, &mpz_fdiv_q_2exp);
}

pointer IntInf_lshift(pointer arg, uint shift, uint bytes) {
        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_lshift (0x%08x, %u, %u)\n",
                                (uint)arg, shift, bytes);
        return shary(arg, shift, bytes, &mpz_mul_2exp);
}

Word
IntInf_smallMul(Word lhs, Word rhs, pointer carry)
{
        llong        prod;

        prod = (llong)(int)lhs * (int)rhs;
        *(uint *)carry = (ullong)prod >> 32;
        return ((uint)(ullong)prod);
}

/*
 * Return an integer which compares to 0 as the two intInf args compare
 * to each other.
 */
Int IntInf_compare (pointer lhs, pointer rhs) {
        __mpz_struct                lhsmpz,
                                rhsmpz;
        mp_limb_t                lhsspace[2],
                                rhsspace[2];

        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_compare (0x%08x, 0x%08x)\n",
                                (uint)lhs, (uint)rhs);
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
pointer IntInf_toString (pointer arg, int base, uint bytes) {
        strng                *sp;
        __mpz_struct        argmpz;
        mp_limb_t        argspace[2];
        char                *str;
        uint                size;
        int                i;
        char                c;

        if (DEBUG_INT_INF)
                fprintf (stderr, "IntInf_toString (0x%08x, %d, %u)\n",
                                (uint)arg, base, bytes);
        assert (base == 2 || base == 8 || base == 10 || base == 16);
        fill (arg, &argmpz, argspace);
        sp = (strng*)gcState.frontier;
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
        sp->card = size;
        sp->magic = STRMAGIC;
        setFrontier (&sp->chars[wordAlign(size)], bytes);
        return (pointer)str;
}

/*
 * Quotient (round towards 0, remainder is returned by IntInf_rem).
 * space is a word array with enough space for the quotient
 *        num limbs + 1 - den limbs
 * shifted numerator
 *        num limbs + 1
 * and shifted denominator
 *        den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer IntInf_quot (pointer num, pointer den, uint bytes) {
        __mpz_struct        resmpz,
                        nmpz,
                        dmpz;
        mp_limb_t        nss[2],
                        dss[2],
                        carry,
                        *np,
                        *dp;
        int                nsize,
                        dsize,
                        qsize;
        bool                resIsNeg;
        uint                shift;

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
 *        den limbs
 * shifted numerator
 *        num limbs + 1
 * and shifted denominator
 *        den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer IntInf_rem (pointer num, pointer den, uint bytes) {
        __mpz_struct        resmpz,
                        nmpz,
                        dmpz;
        mp_limb_t        nss[2],
                        dss[2],
                        carry,
                        *dp;
        int                nsize,
                        dsize;
        bool                resIsNeg;
        uint                shift;

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
