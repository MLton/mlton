#include "platform.h"

Int IEEEReal_getRoundingMode () {
	return fegetround ();
}

void IEEEReal_setRoundingMode (Int m) {
	assert (m != FE_NOSUPPORT);
	fesetround (m);
}
