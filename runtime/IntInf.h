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

/*
 * IntInf_init() is passed an array of struct intInfInit's (along
 * with a pointer to the current GC_state) at the start of the program.
 * The array is terminated by an intInfInit with mlstr field NULL.
 * For each other entry, the globalIndex'th entry of the globals array in
 * the GC_state structure is set to the IntInf.int whose value corresponds
 * to the mlstr string.
 * On return, the GC_state must have been adjusted to account for any space
 * used.
 */
struct intInfInit {
	Word	globalIndex;
	char	*mlstr;
};

extern void	IntInf_init(GC_state state, struct intInfInit inits[]);

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
				IntInf_do_neg(pointer arg,
					      uint bytes),
				IntInf_do_quot(pointer num,
					       pointer den,
					       uint bytes),
				IntInf_do_rem(pointer num,
					      pointer den,
					      uint bytes),
				IntInf_do_andb(pointer lhs,
					       pointer rhs,
					       uint bytes),
				IntInf_do_orb(pointer lhs,
					      pointer rhs,
					      uint bytes),
				IntInf_do_xorb(pointer lhs,
					       pointer rhs,
					       uint bytes),
				IntInf_do_notb(pointer arg,
					       uint bytes),
				IntInf_do_arshift(pointer arg,
						  uint shift,
						  uint bytes),
				IntInf_do_lshift(pointer arg,
						 uint shift,
						 uint bytes),
				IntInf_do_toString(pointer arg,
						   int base,
						   uint bytes),
				IntInf_do_gcd(pointer lhs,
					      pointer rhs,
					      uint bytes);

extern Word	IntInf_smallMul(Word lhs, Word rhs, pointer carry);
extern int	IntInf_compare(pointer lhs, pointer rhs),
		IntInf_equal(pointer lhs, pointer rhs);

#endif	/* #ifndef _MLTON_INT_INF_H */






