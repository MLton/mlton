/* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 */
/*
 * Macros and procedure declarations used by the IntInf support (bignums).
 */

#ifndef	_MLTON_INT_INF_H
#define	_MLTON_INT_INF_H

#include "gc.h"
#include "mlton-basis.h"

/*
 * A pointer to a struct intInfRes_t is used to communicate the state of the
 * world back from some of the C support routines and the running MLton world.
 * The frontier slot holds the new heap frontier and the value slot holds the
 * result.  In cases where some storage might be needed, the ML code allocates
 * the maximum amount which might be needed and calls the C routine.  It then
 * uses what it must, possibly rolling the heap frontier back.
 */
struct	intInfRes_t {
	Pointer	frontier,
		value;
};

/*
 * IntInf_init() is passed an array of struct intInfInit's (along
 * with a Pointer to the current GC_state) at the start of the program.
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
extern struct intInfRes_t	*IntInf_do_add(Pointer lhs,
					     Pointer rhs,
					     Pointer rspace,
					     Pointer frontier),
				*IntInf_do_sub(Pointer lhs,
					     Pointer rhs,
					     Pointer rspace,
					     Pointer frontier),
				*IntInf_do_mul(Pointer lhs,
					     Pointer rhs,
					     Pointer rspace,
					     Pointer frontier),
				*IntInf_do_toString(Pointer arg,
					       int base,
					       Pointer space,
					       Pointer frontier),
				*IntInf_do_neg(Pointer arg,
						Pointer space,
						Pointer frontier),
				*IntInf_do_quot(Pointer num,
					      Pointer den,
					      Pointer space,
					      Pointer frontier),
				*IntInf_do_rem(Pointer num,
					     Pointer den,
					     Pointer space,
					     Pointer frontier);
extern Word	IntInf_smallMul(Word lhs, Word rhs, Pointer carry);
extern int	IntInf_compare(Pointer lhs, Pointer rhs),
		IntInf_equal(Pointer lhs, Pointer rhs);

#endif	/* #ifndef _MLTON_INT_INF_H */






