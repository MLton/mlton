#define _ISOC99_SOURCE

#include "platform.h"

Int IEEEReal_getRoundingMode () {
	switch (fegetround ()) {
#ifdef FE_TONEAREST
	case FE_TONEAREST: return 0;
#endif
#ifdef FE_DOWNWARD
	case FE_DOWNWARD: return 1;
#endif
#ifdef FE_UPWARD
	case FE_UPWARD: return 2;
#endif
#ifdef FE_TOWARDZERO
	case FE_TOWARDZERO: return 3;
#endif
	default:
		die ("IEEEReal_getRoundingMode error: impossible mode.\n");
	}
}

void IEEEReal_setRoundingMode (Int mode) {
	int m;

	switch (mode) {
#ifdef FE_TONEAREST
	case 0: m = FE_TONEAREST; break;
#endif
#ifdef FE_DOWNWARD
	case 1: m = FE_DOWNWARD; break;
#endif
#ifdef FE_UPWARD
	case 2: m = FE_UPWARD; break;
#endif
#ifdef FE_TOWARDZERO
	case 3: m = FE_TOWARDZERO; break;
#endif
	default:
		die ("IEEEReal_setRoundingMode error: unsupported mode %d\n",
			(int)mode);
	}
	fesetround (m);
}
