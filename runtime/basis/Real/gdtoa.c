#define _ISOC99_SOURCE

#include "platform.h"
#include "gdtoa/gdtoa.h"

#ifndef DEBUG
#define DEBUG FALSE
#endif

/* This code is patterned on g_dfmt from the gdtoa sources. */
char * Real64_gdtoa (double d, int mode, int ndig, int *decpt) {
	ULong bits[2];
	int ex;
	static FPI fpi = { 53, 1-1023-53+1, 2046-1023-53+1, 1, 0 };
	int i;
	ULong *L;
	char *result;
	ULong sign;
	int x0, x1;
	
	if (MLton_Platform_Arch_bigendian) {
		x0 = 0;
		x1 = 1;
	} else {
		x0 = 1;
		x1 = 0;
	}

	L = (ULong*)&d;
	sign = L[x0] & 0x80000000L;
	bits[0] = L[x1];
	bits[1] = L[x0] & 0xfffff;
	if ( (ex = (L[x0] >> 20) & 0x7ff) !=0)
		bits[1] |= 0x100000;
	else
		ex = 1;
	ex -= 0x3ff + 52;
	i = STRTOG_Normal;
	result = gdtoa (&fpi, ex, bits, &i, mode, ndig, decpt, NULL);
	if (DEBUG)
		fprintf (stderr, "%s = gdtoa (%g, %d, %d)   decpt = %d\n", 
				result, d, mode, ndig, *decpt);
	return result;
}

char * Real32_gdtoa (float f, int mode, int ndig, int *decpt) {
	return Real64_gdtoa ((double)f, mode, ndig, decpt);
}
