#include "mlton-basis.h"

/* See the comment in quot.c. */

Int32 Int32_rem (Int32 n, Int32 d) {
#if (defined (__i386__) || defined (__sparc__))
	return n % d;
#else
#error check that C % correctly implements Int32.rem from the basis library
#endif
}
