#include "mlton-basis.h"
#include "my-lib.h"

Char Word8_arshiftAsm(Char w, Word s) {
	register uchar cl asm("cl");
	
	cl = s;
	__asm__ __volatile__ ("sarb %%cl, %0"
			      : "=m" (w)
			      : "m" (w), "r" (cl));
	return w;
}
