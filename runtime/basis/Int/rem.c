#include "mlton-basis.h"

/* See the comment in quot.c. */

Int Int_rem (Int n, Int d) {
#if (defined (__i386__))
	return n % d;
#else
#error check that C % correctly implements Int.rem from the basis library
#endif
}
