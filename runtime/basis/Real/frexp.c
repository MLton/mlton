#include <math.h>
#include "mlton-basis.h"

double frexp(double x, int* exp);

Real32 Real32_frexp(Real32 x, Int *exp) {
	int exp_;
        Real32 res;
	res = (Real32)(frexp((Real64) x, &exp_));
	*exp = exp_;
	return res;
}

Real64 Real64_frexp(Real64 x, Int *exp) {
	int exp_;
        Real64 res;
	res = frexp(x, &exp_);
	*exp = exp_;
	return res;
}
