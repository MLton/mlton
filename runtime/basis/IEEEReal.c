#include "platform.h"

/* ------------------------------------------------- */
/*                     IEEEReal                      */
/* ------------------------------------------------- */

#if (defined (__i386__))

/* Macros for accessing the hardware control word.  */
#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))

#define ROUNDING_CONTROL_MASK 0x0C00
#define ROUNDING_CONTROL_SHIFT 10

void IEEEReal_setRoundingMode (Int mode) {
	unsigned short controlWord;

	_FPU_GETCW(controlWord);
	controlWord &= ~ROUNDING_CONTROL_MASK;
	controlWord |= mode << ROUNDING_CONTROL_SHIFT;
	_FPU_SETCW(controlWord);
}

Int IEEEReal_getRoundingMode () {
	unsigned short controlWord;

	_FPU_GETCW(controlWord);
	return (controlWord & ROUNDING_CONTROL_MASK) >> ROUNDING_CONTROL_SHIFT;
}

#elif (defined (__sparc__))

#include <ieeefp.h>

void IEEEReal_setRoundingMode (Int mode) {
	switch (mode) {
	case 0: mode = FP_RN; break;
	case 1: mode = FP_RM; break;
	case 2: mode = FP_RP; break;
	case 3: mode = FP_RZ; break;
	default:
		die ("IEEEReal_setRoundingMode error: invalid mode %d\n", 
			(int)mode);
	}
	fpsetround (mode);
}
 
Int IEEEReal_getRoundingMode () {
	int mode;

	mode = fpgetround ();
	switch (mode) {
	case FP_RN: mode = 0; break;
	case FP_RM: mode = 1; break;
 	case FP_RP: mode = 2; break;
	case FP_RZ: mode = 3; break;
	default:
		die ("IEEEReal_setRoundingMode error: invalid mode %d\n", mode);
	}
	return mode;
}


#endif
