#include "libmlton.h"

enum {
	DEBUG = FALSE,
};

#define Word64_max (Word64)0x7FFFFFFFFFFFFFFF
#define Word64_min (Word64)0x8000000000000000

#define coerce(f, t)				\
	t f##_to##t (f x) {			\
		return (t)x;			\
	}
coerce (WordS16, Word64)
coerce (WordS32, Word64)
coerce (WordS8, Word64)
coerce (WordU16, Word64)
coerce (WordU32, Word64)
coerce (WordU64, Word16)
coerce (WordU64, Word32)
coerce (WordU64, Word8)
coerce (WordU8, Word64)
#undef coerce

#define binary(name, op)				\
	Word64 Word64_##name (Word64 w1, Word64 w2) {	\
		return w1 op w2;			\
	}
binary (add, +)
binary (andb, &)
binary (orb, |)
binary (sub, -)
binary (xorb, ^)
#undef binary

#define binary(kind, name, op)							\
	Word64 Word##kind##64_##name (Word##kind##64 w1, Word##kind##64 w2) {	\
		Word##kind##64 res = w1 op w2;					\
		if (DEBUG)							\
			fprintf (stderr, "%lld = " #name " (%lld, %lld)\n",	\
					res, w1, w2);				\
		return res;							\
	}
binary (S, mul, *)
binary (U, mul, *)
binary (S, quot, /)
binary (U, quot, /)
binary (S, rem, %)
binary (U, rem, %)
#undef binary

#define unary(name, op)				\
	Word64 Word64_##name (Word64 w) {	\
		return op w;			\
	}
unary (neg, -)
unary (notb, ~)
#undef unary

Bool Word64_equal (Word64 w1, Word64 w2) {
	Bool res = w1 == w2;
	if (DEBUG)
		fprintf (stderr, "%s = %llu == %llu\n", 
			res ? "true" : "false", w1, w2);
	return res;
}

#define compare(s, name, op)						\
	Bool Word##s##64_##name (Word##s##64 w1, Word##s##64 w2) {	\
		return w1 op w2;					\
	}
compare (S, ge, >=)
compare (U, ge, >=)
compare (S, gt, >)
compare (U, gt, >)
compare (S, le, <=)
compare (U, le, <=)
compare (S, lt, <)
compare (U, lt, <)
#undef compare

#define shift(size,name, op)					\
	Word64 Word##size##_##name (Word##size w1, Word w2) {	\
		return w1 op w2;				\
	}
shift (64, lshift, <<)
shift (S64, rshift, >>)
shift (U64, rshift, >>)
#undef shift

Word64 Word64_rol (Word64 w1, Word w2) {
	return (w1 >> (64 - w2)) | (w1 << w2);
}

Word64 Word64_ror (Word64 w1, Word w2) {
	return (w1 >> w2) | (w1 << (64 - w2));
}
