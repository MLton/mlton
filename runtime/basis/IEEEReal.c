#include "mlton-basis.h"

/* ------------------------------------------------- */
/*                     IEEEReal                      */
/* ------------------------------------------------- */

#define ROUNDING_CONTROL_MASK 0x0C00
#define ROUNDING_CONTROL_SHIFT 10

void IEEEReal_setRoundingMode(int mode) {
	unsigned short controlWord;

	__asm__ __volatile__ ("fstcw %0"
			: "=m" (controlWord)
			: );
	controlWord =  
		(mode << ROUNDING_CONTROL_SHIFT) 
		| (controlWord & ~ROUNDING_CONTROL_MASK);

	__asm__ __volatile__ ("fldcw %0"
                        :
			: "m" (controlWord));
}

Int IEEEReal_getRoundingMode() {
	unsigned short controlWord;

	__asm__ __volatile__ ("fstcw %0"
			: "=m" (controlWord)
			: );

	return (controlWord & ROUNDING_CONTROL_MASK) >> ROUNDING_CONTROL_SHIFT;
}
