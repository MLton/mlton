#include "mlton-basis.h"

#include "my-lib.h"

Word Word32_addOverflow(Word lhs, Word rhs, Bool *overflow) {
	unsigned long long	tmp;

	tmp = (unsigned long long)lhs + rhs;
	*overflow = (tmp != (Word)tmp);
	return tmp;
}
