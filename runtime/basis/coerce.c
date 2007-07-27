#include "platform.h"

#include "coerce.h"

/* Real coercions depend on rounding mode and can't be inlined where
 * gcc might constant-fold them. 
 */

#define coerce(n, f, t)                         \
  t f##_##n##To##t (f x) {                      \
    return (t)x;                                \
  }
#define bothFromWordCoerce(name, from, to)      \
coerce (name, Word##S##from, to)                \
coerce (name, Word##U##from, to)

#define allWordCoerce(size)                     \
bothFromWordCoerce(rnd, size, Real32)           \
bothFromWordCoerce(rnd, size, Real64)

allWordCoerce(8)
allWordCoerce(16)
allWordCoerce(32)
allWordCoerce(64)

#undef allWordCoerce
#undef bothToWordCoerce
#undef bothFromWordCoerce

#undef coerce
