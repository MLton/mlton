/* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
#elif (defined (__NetBSD__))
/* On NetBSD, we want gmp to be installed into the pkg tree (which represents
 * the FreeBSD ports tree). For now we use the same method as in the FreeBSD
 * case, but we note that this should be changed so the makefile provides the
 * correct -I flags to the compiler
 */
#include "/usr/pkg/include/gmp.h"
#elif (defined (__linux__) || defined (__sun__))
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
 * Immediately after the bytesArg, they take a labelIndex arg.  This is an index
 * into the array used for allocation profiling, and the appropriate element
 * is incremented by the amount that the function moves the frontier.
 */
extern pointer			IntInf_add(pointer lhs,
					      pointer rhs,
					      uint bytes),
				IntInf_sub(pointer lhs,
					      pointer rhs,
					      uint bytes),
				IntInf_mul(pointer lhs,
					      pointer rhs,
					      uint bytes),
				IntInf_neg(pointer arg,
					      uint bytes),
				IntInf_quot(pointer num,
					       pointer den,
					       uint bytes),
				IntInf_rem(pointer num,
					      pointer den,
					      uint bytes),
				IntInf_andb(pointer lhs,
					       pointer rhs,
					       uint bytes),
				IntInf_orb(pointer lhs,
					      pointer rhs,
					      uint bytes),
				IntInf_xorb(pointer lhs,
					       pointer rhs,
					       uint bytes),
				IntInf_notb(pointer arg,
					       uint bytes),
				IntInf_arshift(pointer arg,
						  uint shift,
						  uint bytes),
				IntInf_lshift(pointer arg,
						 uint shift,
						 uint bytes),
				IntInf_toString(pointer arg,
						   int base,
						   uint bytes),
				IntInf_gcd(pointer lhs,
					      pointer rhs,
					      uint bytes);

extern Word	IntInf_smallMul(Word lhs, Word rhs, pointer carry);
extern int	IntInf_compare(pointer lhs, pointer rhs),
		IntInf_equal(pointer lhs, pointer rhs);

#endif	/* #ifndef _MLTON_INT_INF_H */






