#include <math.h>
#include "mlton-basis.h"

Real32 Real32_modf(Real32 x, Real32 *exp) {
	Real64 exp_, res;
        res = modf((Real64) x, &exp_);
        *exp = (Real32)(exp_);
	return (Real32)(res);
}

Real64 Real64_modf(Real64 x, Real64 *exp) {
	return modf(x, exp);
}
