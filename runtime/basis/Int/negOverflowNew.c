#include "mlton-basis.h"
#include "my-lib.h"

Bool Int_negOverflowNew(Int n, Bool *overflow) {
	long long	tmp;

	tmp = -(long long)n;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
