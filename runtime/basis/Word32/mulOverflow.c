#include "mlton-basis.h"

#include "my-lib.h"

Word Word32_mulOverflow(Word lhs, Word rhs, Bool *overflow) {
	unsigned long long	tmp;

	tmp = (unsigned long long)lhs * rhs;
	*overflow = (tmp != (Word)tmp);
	if (FALSE)
		fprintf (stderr, "0x%x * 0x%x = 0x%llu (overflow = %d)\n",
				(uint)lhs, (uint)rhs, tmp, *overflow);
	return tmp;
}
