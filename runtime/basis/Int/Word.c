#include "platform.h"

/*
 * We have to be very careful implementing WordS_quot and WordS_rem using / and %
 * because C allows
 *  "The direction of truncation for / and the sign of the result for % are
 *   machine-dependent for negative operands, ..." (K&R p. 41) (See also p. 205.)
 * On the other hand, the SML Basis library spec is completely nailed down.
 * On x86, gcc implements / and % using idiv, which fortunately does have the
 * same semantics as the SML Basis library.  However, gcc's optimizer sometimes
 * takes advantage of the flexibility in the C spec when one of the arguments
 * is a constant, producing incorrect results.  So, we have two options:
 *
 * Put them in a separate file, all by themselves, without a static inline, and
 * use / and % where we know gcc's optimer can't hurt us.
 * OR
 * Use inline assembler.
 *
 * We've gone for the first option because of simplicity, and because
 * quot and rem are only used with the C codegen.  If you really want
 * speed, you could try inline assembler.
 *
 * To get this working on another architecture, you need to check how gcc
 * implements / and %.
 */

#ifndef DEBUG
#define DEBUG FALSE
#endif

#if ! (defined (__i386__) || defined (__sparc__))
#error check that C {/,%} correctly implement {quot,rem} from the basis library
#endif

#define coerce(f, t)				\
	t f##_to##t (f x) {			\
		return (t)x;			\
	}

#define bothCoerce(from, to)			\
	coerce (Word##S##from, Word##to)	\
	coerce (Word##U##from, Word##to)

#define binary(kind, name, op)						\
	Word##kind Word##kind##_##name (Word##kind w1, Word##kind w2) {	\
		return w1 op w2;					\
	}

#define bothBinary(size, name, op)		\
	binary (S##size, name, op)		\
	binary (U##size, name, op)

#define compare(kind, name, op)						\
	Bool Word##kind##_##name (Word##kind w1, Word##kind w2) {	\
		return w1 op w2;					\
	}

#define bothCompare(size, name, op)		\
	compare (S##size, name, op)		\
	compare (U##size, name, op)

#define unary(kind,name, op)				\
	Word##kind Word##kind##_##name (Word##kind w) {	\
		return op w;				\
	}

#define shift(kind, name, op)						\
	Word##kind Word##kind##_##name (Word##kind w1, Word w2) {	\
		return w1 op w2;					\
	}

#define all(size)						\
	binary (size, add, +)					\
	binary (size, andb, &)					\
	compare (size, equal, ==)				\
	bothCompare (size, ge, >=)				\
	bothCompare (size, gt, >)				\
	bothCompare (size, le, <=)				\
	shift (size, lshift, <<)				\
	bothCompare (size, lt, <)				\
	bothBinary (size, mul, *)				\
	unary (size, neg, -)					\
	unary (size, notb, ~)					\
	binary (size, orb, |)					\
	bothBinary (size, quot, /)				\
	bothBinary (size, rem, %)				\
	Word##size Word##size##_rol (Word##size w1, Word w2) {	\
		return (w1 >> (size - w2)) | (w1 << w2);	\
	}							\
	Word##size Word##size##_ror (Word##size w1, Word w2) {	\
		return (w1 >> w2) | (w1 << (size - w2));	\
	}							\
	shift (S##size, rshift, >>)				\
	shift (U##size, rshift, >>)				\
	binary (size, sub, -)					\
	binary (size, xorb, ^)					\
	bothCoerce (size, 64)					\
	bothCoerce (size, 32)					\
	bothCoerce (size, 16)					\
	bothCoerce (size, 8)

all (8)
all (16)
all (32)
all (64)

#undef coerce
#undef bothCoerce
#undef binary
#undef bothBinary
#undef compare
#undef bothCompare
#undef unary
#undef shift
#undef all
