#include "mlton-basis.h"

/* See the comment in quot.c. */

#if ! (defined (__i386__) || defined (__sparc__))
#error check that C % correctly implements rem from the basis library
#endif

Int8 Int8_rem (Int8 n, Int8 d) {
	return n % d;
}

Int16 Int16_rem (Int16 n, Int16 d) {
	return n % d;
}

Int32 Int32_rem (Int32 n, Int32 d) {
	return n % d;
}
