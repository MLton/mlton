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

#if ! (defined (__amd64__) || defined (__hppa__) || defined (__i386__) || defined(__ia64__) || defined(__mips__) || defined (__ppc__) || defined (__powerpc__) || defined (__sparc__))
#error check that C {/,%} correctly implement {quot,rem} from the basis library
#endif

#include "Word-consts.h"
#include "Word-ops.h"
#include "Word-check.h"
