/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */
#include "gmp.h"
#include "IntInf.h"

#include <string.h>


/*
 * Second header word for bignums and strings.
 */
#define	BIGMAGIC	GC_arrayHeader(4, 0)
#define	STRMAGIC	GC_arrayHeader(1, 0)


/*
 * Layout of strings.  Note, the value passed around is a pointer to
 * the chars member.
 */
typedef struct	strng {
	uint	card,		/* number of chars */
		magic;		/* STRMAGIC */
	char	chars[0];	/* actual chars */
}	strng;


/*
 * Layout of bignums.  Note, the value passed around is a pointer to
 * the isneg member.
 */
typedef struct	bignum {
	uint	card,		/* one more than the number of limbs */
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
 * Convert a pointer to a strng pointer.
 */
static inline strng	*
toString(pointer arg)
{
	strng	*sp;

	assert(not isSmall(arg));
	sp = (strng *)((uint)arg - 2*sizeof(uint));
	assert(sp->magic == STRMAGIC);
	return (sp);
}


/*
 * Convert a bignum intInf to a bignum pointer.
 */
static inline bignum	*
toBignum(pointer arg)
{
	bignum	*bp;

	assert(not isSmall(arg));
	bp = (bignum *)((uint)arg - 2*sizeof(uint));
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
init(bignum *bp, __mpz_struct *mpzp)
{
	assert(bp->card > 1);
	mpzp->_mp_alloc = bp->card - 1;
	mpzp->_mp_size = 0;
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
 * Given an __mpz_struct pointer which reflects the answer, and a
 * struct intInfRes_t pointer which is the actual answer, fill in the latter.
 * If the answer fits in a fixnum, we return that, with the frontier
 * rolled back.
 * If the answer doesn't need all of the space allocated, we adjust
 * the array size and roll the frontier slightly back.
 * Note, this all assumes that the last thing allocated was the array
 * which is used for space by the __mpz_struct.
 */
static void
answer(__mpz_struct *ans, struct intInfRes_t *res)
{
	bignum			*bp;
	int			size;

	bp = (bignum *)&ans->_mp_d[-3];
	assert(ans->_mp_d == bp->limbs);
	assert(ans->_mp_alloc == bp->card - 1);
	assert(bp->magic == BIGMAGIC);
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
			ans = ans<<1 | 1;
			res->value = (pointer)ans;
			res->frontier = (pointer)bp;
			return;
		}
	}
	res->value = (pointer)&bp->isneg;
	res->frontier = (pointer)&bp->limbs[size];
	unless (size == ans->_mp_alloc)
		GC_arrayShrink((pointer)res->value, size+1);
}


struct intInfRes_t	*
IntInf_do_add(pointer lhs, pointer rhs, pointer rspace, pointer frontier)
{
	bignum		*bp;
	__mpz_struct	lhsmpz,
			rhsmpz,
			resmpz;
	mp_limb_t	lhsspace[2],
			rhsspace[2];
	static struct intInfRes_t	res;

	bp = toBignum(rspace);
	assert(frontier == (pointer)&bp->limbs[bp->card - 1]);
	fill(lhs, &lhsmpz, lhsspace);
	fill(rhs, &rhsmpz, rhsspace);
	init(bp, &resmpz);
	mpz_add(&resmpz, &lhsmpz, &rhsmpz);
	assert((resmpz._mp_alloc < bp->card)
	and (resmpz._mp_d == bp->limbs));
	answer(&resmpz, &res);
	assert((pointer)bp <= res.frontier);
	return (&res);
}


struct intInfRes_t	*
IntInf_do_sub(pointer lhs, pointer rhs, pointer rspace, pointer frontier)
{
	bignum		*bp;
	__mpz_struct	lhsmpz,
			rhsmpz,
			resmpz;
	mp_limb_t	lhsspace[2],
			rhsspace[2];
	static struct intInfRes_t	res;

	bp = toBignum(rspace);
	assert(frontier == (pointer)&bp->limbs[bp->card - 1]);
	fill(lhs, &lhsmpz, lhsspace);
	fill(rhs, &rhsmpz, rhsspace);
	init(bp, &resmpz);
	mpz_sub(&resmpz, &lhsmpz, &rhsmpz);
	assert((resmpz._mp_alloc < bp->card)
	and (resmpz._mp_d == bp->limbs));
	answer(&resmpz, &res);
	assert((pointer)bp <= res.frontier);
	return (&res);
}


uint
IntInf_smallMul(uint lhs, uint rhs, pointer carry)
{
	llong	prod;

	prod = (llong)(int)lhs * (int)rhs;
	*(uint *)carry = (ullong)prod >> 32;
	return ((uint)(ullong)prod);
}


struct intInfRes_t	*
IntInf_do_mul(pointer lhs, pointer rhs, pointer rspace, pointer frontier)
{
	bignum		*bp;
	__mpz_struct	lhsmpz,
			rhsmpz,
			resmpz;
	mp_limb_t	lhsspace[2],
			rhsspace[2];
	static struct intInfRes_t	res;

	bp = toBignum(rspace);
	assert(frontier == (pointer)&bp->limbs[bp->card - 1]);
	fill(lhs, &lhsmpz, lhsspace);
	fill(rhs, &rhsmpz, rhsspace);
	init(bp, &resmpz);
	mpz_mul(&resmpz, &lhsmpz, &rhsmpz);
	assert((resmpz._mp_alloc < bp->card)
	and (resmpz._mp_d == bp->limbs));
	answer(&resmpz, &res);
	assert((pointer)bp <= res.frontier);
	return (&res);
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
struct intInfRes_t	*
IntInf_do_toString(pointer arg, int base, pointer space, pointer frontier)
{
	strng		*sp;
	__mpz_struct	argmpz;
	mp_limb_t	argspace[2];
	char		*str;
	uint		size;
	static struct intInfRes_t	res;

	assert(base == 2 || base == 8 || base == 10 || base == 16);
	fill(arg, &argmpz, argspace);
	sp = toString(space);
	str = mpz_get_str(sp->chars, base, &argmpz);
	assert(str == sp->chars);
	size = strlen(str);
	assert(0 < size && size < sp->card);
	if (*sp->chars == '-')
		*sp->chars = '~';
	GC_arrayShrink(space, size);
	size += sizeof(pointer) - 1;
	size -= size % sizeof(pointer);
	assert(frontier >= &sp->chars[size]);
	res.frontier = &sp->chars[size];
	res.value = space;
	return (&res);
}


struct intInfRes_t	*
IntInf_do_neg(pointer arg, pointer space, pointer frontier)
{
	bignum		*bp;
	__mpz_struct	argmpz,
			resmpz;
	mp_limb_t	argspace[2];
	static struct intInfRes_t	res;

	bp = toBignum(space);
	assert(frontier == (pointer)&bp->limbs[bp->card - 1]);
	fill(arg, &argmpz, argspace);
	init(bp, &resmpz);
	mpz_neg(&resmpz, &argmpz);
	assert((resmpz._mp_alloc < bp->card)
	and (resmpz._mp_d == bp->limbs));
	answer(&resmpz, &res);
	assert((pointer)bp <= res.frontier);
	return (&res);
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
struct intInfRes_t	*
IntInf_do_quot(pointer num, pointer den, pointer space, pointer frontier)
{
	bignum		*spbp;
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
	static struct intInfRes_t	res;

	spbp = toBignum(space);
	assert(frontier == (pointer)&spbp->limbs[spbp->card - 1]);
	init(spbp, &resmpz);
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
		if (nsize == 0) {
			res.value = (pointer)1;	/* tagged 0 */
			res.frontier = (pointer)spbp;
			return (&res);
		}
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
			assert(&np[nsize] <= &spbp->limbs[spbp->card - 1]);
			memcpy((void *)np,
			       nmpz._mp_d,
			       nsize * sizeof(*nmpz._mp_d));
		} else {
			carry = mpn_lshift(np, nmpz._mp_d, nsize, shift);
			unless (carry == 0)
				np[nsize++] = carry;
			dp = &np[nsize];
			assert(&dp[dsize] <= &spbp->limbs[spbp->card - 1]);
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
	answer(&resmpz, &res);
	assert((pointer)spbp <= res.frontier);
	return (&res);
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
struct intInfRes_t	*
IntInf_do_rem(pointer num, pointer den, pointer space, pointer frontier)
{
	bignum		*spbp;
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
	static struct intInfRes_t	res;

	spbp = toBignum(space);
	assert(frontier == (pointer)&spbp->limbs[spbp->card - 1]);
	init(spbp, &resmpz);
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
			assert(&resmpz._mp_d[nsize] <= &spbp->limbs[spbp->card - 1]);
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
			assert(&dp[dsize] <= &spbp->limbs[spbp->card - 1]);
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
	answer(&resmpz, &res);
	assert((pointer)spbp <= res.frontier);
	return (&res);
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
		bp->card = alen + 1;
		bp->magic = BIGMAGIC;
		bp->isneg = neg;
		state->frontier = (pointer)&bp->limbs[alen];
	}
}






