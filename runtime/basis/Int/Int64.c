#include "libmlton.h"

enum {
	DEBUG = FALSE,
};

#define Int64_max (Int64)0x7FFFFFFFFFFFFFFF
#define Int64_min (Int64)0x8000000000000000

#define binary(name, op)							\
	Int64 Int64_##name (Int64 i, Int64 j) {					\
		if (DEBUG)							\
			fprintf (stderr, "%lld = " #name " (%lld, %lld)\n",	\
					i op j, i, j);				\
		return i op j;							\
	}
binary(add, +)
binary(mul, *)
binary(sub, -)
binary(quot, /)
binary(rem, %)
#undef binary

#define compare(name, op)						\
	Bool Int64_##name (Int64 i, Int64 j) {				\
		if (DEBUG)						\
			fprintf (stderr, "%d = %lld " #op " %lld\n",	\
					i op j, i, j);			\
		return i op j;						\
	}
compare(equal, ==)
compare(ge, >=)
compare(gt, >)
compare(le, <=)
compare(lt, <)
#undef compare

#define compareU(name,op)			\
	Bool Int64_##name (Int64 i, Int64 j) {	\
		return (Word64)i op (Word64)j;	\
	}
compareU(geu, >=)
compareU(gtu, >)
#undef compareU

Int32 Int64_toInt32 (Int64 i) {
	return (Int32)i;
}

Int64 Int32_toInt64 (Int32 i) {
	return (Int64)i;
}

Word32 Int64_toWord32 (Int64 i) {
	return (Word32)i;
}

Int64 Word32_toInt64 (Word32 i) {
	return (Int64)i;
}
