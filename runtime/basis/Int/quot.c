#include <stdio.h>

#include "mlton-basis.h"

Int Int_quot(Int numerator, Int denominator) {
	register int eax asm("ax");

	eax = numerator ;
	
	__asm__ __volatile__ ("cdq\n        idivl %1"
		: 
		: "r" (eax), "m" (denominator)
		: "eax", "edx");

	return eax;
}
