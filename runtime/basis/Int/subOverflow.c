#include "mlton-basis.h"

#include "my-lib.h"

Bool Int_subOverflow(Int lhs, Int rhs, Int *res) {
	long long	tmp;

	tmp = (long long)lhs - rhs;
	if (tmp != (int)tmp)
		return (TRUE);
	*res = tmp;
	return (FALSE);
}
