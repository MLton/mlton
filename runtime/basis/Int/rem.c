#include "mlton-basis.h"

Int Int_rem(Int numerator, Int denominator) {
	register int eax asm("ax"),
			edx asm("dx");
	
	eax = numerator ;
	
	__asm__ __volatile__ ("cdq\n        idivl %1"
		: 
		: "r" (eax), "m" (denominator)
		: "eax", "edx");
	
	return edx;
}
