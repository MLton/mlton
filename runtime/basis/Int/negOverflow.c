#include "mlton-basis.h"
#include "my-lib.h"

Int Int_negOverflow(Int n, Bool *overflow) {
	long long	tmp;

	tmp = -(long long)n;
	*overflow = (tmp != (int)tmp);
	return tmp;
}
