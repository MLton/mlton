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
coerce(Int32,Word64)
coerce(Word8,Word64)
coerce(Word16,Word64)
coerce(Word32,Word64)
coerce(Word64,Int32)
coerce(Word64,Word8)
coerce(Word64,Word16)
coerce(Word64,Word32)
#undef coerce

#define coerceX(size, t)				\
	t Word##size##_to##t##X (Word##size w) {	\
		return (t)(Int##size)w;			\
	}
coerceX(8,Word64)
coerceX(16,Word64)
coerceX(32,Word64)
coerceX(64,Int32)
#undef coerceX

#define binary(name, op)				\
	Word64 Word64_##name (Word64 w1, Word64 w2) {	\
		return w1 op w2;			\
	}
binary (add, +)
binary (andb, &)
binary (div, /)
binary (mod, %)
binary (mul, *)
binary (orb, |)
binary (sub, -)
binary (xorb, ^)
#undef binary

#define unary(name, op)				\
	Word64 Word64_##name (Word64 w) {	\
		return op w;			\
	}
unary (neg, -)
unary (notb, ~)
#undef unary

#define compare(name, op)				\
	Bool Word64_##name (Word64 w1, Word64 w2) {	\
		return w1 op w2;			\
	}
compare (equal, ==)
compare (ge, >=)
compare (gt, >)
compare (le, <=)
compare (lt, <)
#undef binary

#define shift(name, op)					\
	Word64 Word64_##name (Word64 w1, Word w2) {	\
		return w1 op w2;			\
	}
shift (lshift, <<)
shift (rshift, >>)
#undef binary

Word64 Word64_arshift (Word64 w, Word s) {
	return (Int64)w >> s;
}

Word64 Word64_rol (Word64 w1, Word w2) {
	return (w1 >> (64 - w2)) | (w1 << w2);
}

Word64 Word64_ror (Word64 w1, Word w2) {
	return (w1 >> w2) | (w1 << (64 - w2));
}
