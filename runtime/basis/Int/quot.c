#include <stdio.h>

#include "mlton-basis.h"

/*
 * We have to be very careful implementing Int_quot and Int_rem using / and %
 * because C allows
 *  "The direction of truncation for / and the sign of the result for % are
 *   machine-dependent for negative operands, ..." (K&R p. 41) (See also p. 205.)
 * On the other hand, the SML Basis library spec is completely nailed down.
 * On x86, gcc implements / and % using idiv, which fortunately does have the
 * same semantics as the SML Basis library.  However, gcc's optimizer sometimes
 * takes advantage of the flexibility in the C spec when one of the arguments
 * is a constant, producing incorrect results.  So, we have two options:
 *
 * Put Int_quot and Int_rem in a separate file, all by themselves, without a
 * static inline, and use / and % where we know gcc's optimer can't hurt us.
 * OR
 * Use inline assembler.
 *
 * We've gone for the first option because of simplicity, and because
 * Int_quot and Int_rem are only used with the C codegen.  If you really want
 * speed, you could try inline assembler.
 *
 * To get this working on another architecture, you need to check how gcc
 * implements / and %.
 */

Int Int_quot (Int n, Int d) {
#if (defined (__i386__))
	return n / d;
#else
#error check that C / correctly implements Int.quot from the basis library
#endif
}
