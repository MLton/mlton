/* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#include "gmp.h"
#include "IntInf.h"

#include <stddef.h> /* for offsetof */
#include <string.h>

/* Import the global gcState so we can get and set the frontier. */
extern struct GC_state gcState;

/*
 * Third header word for bignums and strings.
 */
#define	BIGMAGIC	GC_objectHeader(WORD_VECTOR_TYPE_INDEX)
#define	STRMAGIC	GC_objectHeader(STRING_TYPE_INDEX)


/*
 * Layout of strings.  Note, the value passed around is a pointer to
 * the chars member.
 */
typedef struct	strng {
	uint	counter,	/* used by GC. */
		card,		/* number of chars */
		magic;		/* STRMAGIC */
	char	chars[0];	/* actual chars */
}	strng;


/*
 * Layout of bignums.  Note, the value passed around is a pointer to
 * the isneg member.
 */
typedef struct	bignum {
	uint	counter,	/* used by GC. */
		card,		/* one more than the number of limbs */
		magic,		/* BIGMAGIC */
		isneg;		/* iff bignum is negative */
	ulong	limbs[0];	/* big digits, least significant first */
}	bignum;


/*
 * Test if a intInf is a fixnum.
 */
static inline	uint
isSmall(pointer arg)
{
	return ((uint)arg & 1);
}


/*
 * Convert a bignum intInf to a bignum pointer.
 */
static inline bignum	*
toBignum(pointer arg)
{
	bignum	*bp;

	assert(not isSmall(arg));
	bp = (bignum *)((uint)arg - offsetof(struct bignum, isneg));
	assert(bp->magic == BIGMAGIC);
	return (bp);
}


/*
 * Given an intInf, a pointer to an __mpz_struct and something large enough
 * to contain 2 limbs, fill in the __mpz_struct.
 */
static inline void
fill(pointer arg, __mpz_struct *res, mp_limb_t space[2])
{
	bignum	*bp;

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
static inline void
initRes(__mpz_struct *mpzp, uint bytes)
{
	struct bignum *bp;

	assert(bytes <= gcState.limitPlusSlop - gcState.frontier);
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
static inline uint
leadingZeros(mp_limb_t word)
{
	uint	res;

	assert(word != 0);
	res = 0;
	while ((int)word > 0) {
		++res;
		word <<= 1;
	}
	return (res);
}


/*
 * Given an __mpz_struct pointer which reflects the answer, set gcState.frontier
 * and return the answer.
 * If the answer fits in a fixnum, we return that, with the frontier
 * rolled back.
 * If the answer doesn't need all of the space allocated, we adjust
 * the array size and roll the frontier slightly back.
 */
static pointer
answer(__mpz_struct *ans)
{
	bignum			*bp;
	int			size;

	bp = (bignum *)((pointer)ans->_mp_d - offsetof(struct bignum, limbs));
	assert(ans->_mp_d == bp->limbs);
	size = ans->_mp_size;
	if (size < 0) {
		bp->isneg = TRUE;
		size = - size;
	} else
		bp->isneg = FALSE;
	if (size <= 1) {
		uint	val,
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
	gcState.frontier = (pointer)&bp->limbs[size];
	assert(gcState.frontier <= gcState.limitPlusSlop);
	bp->counter = 0;
	bp->card = size + 1; /* +1 for isNeg word */
	bp->magic = BIGMAGIC;
	return (pointer)&bp->isneg;
}

static pointer
binary(pointer lhs, pointer rhs, uint bytes,
	void(*binop)(__mpz_struct *resmpz, 
			__gmp_const __mpz_struct *lhsspace,
			__gmp_const __mpz_struct *rhsspace))
{
	__mpz_struct	lhsmpz,
			rhsmpz,
			resmpz;
	mp_limb_t	lhsspace[2],
			rhsspace[2];

	initRes(&resmpz, bytes);
	fill(lhs, &lhsmpz, lhsspace);
	fill(rhs, &rhsmpz, rhsspace);
	binop(&resmpz, &lhsmpz, &rhsmpz);
	return answer(&resmpz);
}

pointer IntInf_do_add(pointer lhs, pointer rhs, uint bytes)
{
	return binary(lhs, rhs, bytes, &mpz_add);
}

pointer IntInf_do_gcd(pointer lhs, pointer rhs, uint bytes)
{
	return binary(lhs, rhs, bytes, &mpz_gcd);
}

pointer IntInf_do_mul(pointer lhs, pointer rhs, uint bytes)
{
	return binary(lhs, rhs, bytes, &mpz_mul);
}

pointer IntInf_do_sub(pointer lhs, pointer rhs, uint bytes)
{
	return binary(lhs, rhs, bytes, &mpz_sub);
}

Word
IntInf_smallMul(Word lhs, Word rhs, pointer carry)
{
	llong	prod;

	prod = (llong)(int)lhs * (int)rhs;
	*(uint *)carry = (ullong)prod >> 32;
	return ((uint)(ullong)prod);
}

/*
 * Return an integer which compares to 0 as the two intInf args compare
 * to each other.
 */
int
IntInf_compare(pointer lhs, pointer rhs)
{
	__mpz_struct		lhsmpz,
				rhsmpz;
	mp_limb_t		lhsspace[2],
				rhsspace[2];

	fill(lhs, &lhsmpz, lhsspace);
	fill(rhs, &rhsmpz, rhsspace);
	return (mpz_cmp(&lhsmpz, &rhsmpz));
}


/*
 * Check if two IntInf.int's are equal.
 * (This should be partly in ML, but the compiler won't call ML code in the
 * middle of polymorphic equality.)
 */
int IntInf_equal(pointer lhs, pointer rhs) {
	if (isSmall(lhs))
		if (isSmall(rhs))
			return (lhs == rhs);
		else
			return (FALSE);
	else if (isSmall(rhs))
		return (FALSE);
	else
		return (IntInf_compare(lhs, rhs) == 0);
}


/*
 * Convert an intInf to a string.
 * Arg is an intInf, base is the base to use (2, 8, 10 or 16) and space is a
 * string (mutable) which is large enough.
 */
pointer
IntInf_do_toString(pointer arg, int base, uint bytes)
{
	strng		*sp;
	__mpz_struct	argmpz;
	mp_limb_t	argspace[2];
	char		*str;
	uint		size;

	assert(base == 2 || base == 8 || base == 10 || base == 16);
	fill(arg, &argmpz, argspace);
	sp = (strng*)gcState.frontier;
	str = mpz_get_str(sp->chars, base, &argmpz);
	assert(str == sp->chars);
	size = strlen(str);
	if (*sp->chars == '-')
		*sp->chars = '~';
	sp->counter = 0;
	sp->card = size;
	sp->magic = STRMAGIC;
	gcState.frontier = &sp->chars[wordAlign(size)];
	assert(gcState.frontier <= gcState.limitPlusSlop);
	return (pointer)str;
}

pointer
IntInf_do_neg(pointer arg, uint bytes)
{
	__mpz_struct	argmpz,
			resmpz;
	mp_limb_t	argspace[2];

	initRes(&resmpz, bytes);
	fill(arg, &argmpz, argspace);
	mpz_neg(&resmpz, &argmpz);
	return answer(&resmpz);
}

/*
 * Quotient (round towards 0, remainder is returned by IntInf_rem).
 * space is a word array with enough space for the quotient
 *	num limbs + 1 - den limbs
 * shifted numerator
 *	num limbs + 1
 * and shifted denominator
 *	den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer
IntInf_do_quot(pointer num, pointer den, uint bytes)
{
	__mpz_struct	resmpz,
			nmpz,
			dmpz;
	mp_limb_t	nss[2],
			dss[2],
			carry,
			*np,
			*dp;
	int		nsize,
			dsize,
			qsize;
	bool		resIsNeg;
	uint		shift;

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
	return answer(&resmpz);
}


/*
 * Remainder (sign taken from numerator, quotient is returned by IntInf_quot).
 * space is a word array with enough space for the remainder
 *	den limbs
 * shifted numerator
 *	num limbs + 1
 * and shifted denominator
 *	den limbs
 * and the isNeg word.
 * It must be the last thing allocated.
 * num is the numerator bignum, den is the denominator and frontier is
 * the current frontier.
 */
pointer
IntInf_do_rem(pointer num, pointer den, uint bytes)
{
	__mpz_struct	resmpz,
			nmpz,
			dmpz;
	mp_limb_t	nss[2],
			dss[2],
			carry,
			*dp;
	int		nsize,
			dsize;
	bool		resIsNeg;
	uint		shift;

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
	return answer(&resmpz);
}


/*
 * For each entry { globalIndex, mlstr} in the inits array (which is terminated
 * by one with an mlstr of NULL), set
 *	state->globals[globalIndex]
 * to the corresponding IntInf.int value.
 * On exit, the GC_state pointed to by state is adjusted to account for any
 * space used.
 * The strings pointed to by the mlstr fields consist of
 *	an optional ~
 *	either one or more of [0-9] or
 *		0x followed by one or more of [0-9a-fA-F]
 *	a trailing EOS
 */
void
IntInf_init(GC_state state, struct intInfInit *inits)
{
	char	*str;
	uint	slen,
		llen,
		alen,
		i;
	bool	neg,
		hex;
	bignum	*bp;
	char	*cp;

	for (; (str = inits->mlstr) != NULL; ++inits) {
		assert(inits->globalIndex < state->numGlobals);
		neg = *str == '~';
		if (neg)
			++str;
		slen = strlen(str);
		hex = str[0] == '0' && str[1] == 'x';
		if (hex) {
			str += 2;
			slen -= 2;
			llen = (slen + 7) / 8;
		} else
			llen = (slen + 8) / 9;
		assert(slen > 0);
		bp = (bignum *)state->frontier;
		cp = (char *)&bp->limbs[llen];
		if ((pointer)&cp[slen] >= state->limit)
			die("Out of space");
		for (i = 0; i != slen; ++i)
			if ('0' <= str[i] && str[i] <= '9')
				cp[i] = str[i] - '0' + 0;
			else if ('a' <= str[i] && str[i] <= 'f')
				cp[i] = str[i] - 'a' + 0xa;
			else {
				assert('A' <= str[i] && str[i] <= 'F');
				cp[i] = str[i] - 'A' + 0xA;
			}
		alen = mpn_set_str(bp->limbs, cp, slen, hex ? 0x10 : 10);
		assert(alen <= llen);
		if (alen <= 1) {
			uint	val,
				ans;

			if (alen == 0)
				val = 0;
			else
				val = bp->limbs[0];
			if (neg) {
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
				state->globals[inits->globalIndex] = 
					(pointer)(ans<<1 | 1);
				continue;
			}
		}
		state->globals[inits->globalIndex] = (pointer)&bp->isneg;
		bp->counter = 0;
		bp->card = alen + 1;
		bp->magic = BIGMAGIC;
		bp->isneg = neg;
		state->frontier = (pointer)&bp->limbs[alen];
	}
}
