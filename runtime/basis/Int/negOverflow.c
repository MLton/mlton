#include "mlton-basis.h"
#include "my-lib.h"

Bool Int_negOverflow(Int n, Int *res) {
	long long	tmp;

	tmp = -(long long)n;
	if (tmp != (int)tmp)
		return (TRUE);
	*res = tmp;
	return (FALSE);
}
