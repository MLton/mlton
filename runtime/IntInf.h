/* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
/*
 * Macros and procedure declarations used by the IntInf support (bignums).
 */

#ifndef	_MLTON_INT_INF_H
#define	_MLTON_INT_INF_H

#include "gc.h"
#include "mlton-basis.h"

#if (defined (__CYGWIN__))
#include "gmp.h"
#elif (defined (__FreeBSD__))
/* On FreeBSD, the default gmp.h is installed in /usr/include, but that is
 * version 2.  We want gmp version 4, which the is installed in 
 * /usr/local/include, and is ensured to exist because it is required by the
 * MLton package.
 */
#include "/usr/local/include/gmp.h"
#elif (defined (__linux__))
#include <gmp.h>
#else
#error gmp.h not defined for platform
#endif

/*
 * Third header word for bignums and strings.
 */
#define	BIGMAGIC	GC_objectHeader(WORD_VECTOR_TYPE_INDEX)
#define	STRMAGIC	GC_objectHeader(STRING_TYPE_INDEX)

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

/* All of these routines modify the frontier in gcState.  They assume that 
 * there are bytes bytes free, and allocate an array to store the result
 * at the current frontier position.
 */
extern pointer			IntInf_do_add(pointer lhs,
					     pointer rhs,
					     uint bytes),
				IntInf_do_sub(pointer lhs,
					     pointer rhs,
					     uint bytes),
				IntInf_do_mul(pointer lhs,
					     pointer rhs,
					     uint bytes),
				IntInf_do_toString(pointer arg,
					       int base,
					       uint bytes),
				IntInf_do_neg(pointer arg,
						uint bytes),
				IntInf_do_quot(pointer num,
					      pointer den,
					      uint bytes),
				IntInf_do_rem(pointer num,
					     pointer den,
					     uint bytes),
				IntInf_do_gcd(pointer lhs,
 					     pointer rhs,
					     uint bytes);

extern Word	IntInf_smallMul(Word lhs, Word rhs, pointer carry);
extern int	IntInf_compare(pointer lhs, pointer rhs),
		IntInf_equal(pointer lhs, pointer rhs);

#endif	/* #ifndef _MLTON_INT_INF_H */






