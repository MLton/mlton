#include "platform.h"

Int32 FFI_INT = 13;
Word32 FFI_WORD = 0xFF;
Bool FFI_BOOL = TRUE;
Real64 FFI_REAL = 3.14159;

Char8 ffi (Pointer a1, Pointer a2, Int32 n) {
        double *ds = (double*)a1;
        int *p = (int*)a2;
        int i;
        double sum;

        sum = 0.0;
        for (i = 0; i < GC_getArrayLength (a1); ++i) {
                sum += ds[i];
                ds[i] += n;
        }
        *p = (int)sum;
        return 'c';
}
