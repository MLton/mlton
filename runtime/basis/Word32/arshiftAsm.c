#include "mlton-basis.h"
#include "my-lib.h"

Word Word32_arshiftAsm(Word w, Word s) {
	register uchar cl asm("cl");
	
	__asm__ __volatile__ ("sarl %%cl, %0"
			      : "=m" (w)
			      : "m" (w), "r" (cl));
	return w;
}
