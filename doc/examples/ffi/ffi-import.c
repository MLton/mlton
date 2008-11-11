#include "export.h"

Int32 FFI_INT = 13;
Word32 FFI_WORD = 0xFF;
Bool FFI_BOOL = 1;
Real64 FFI_REAL = 3.14159;

Char8 ffi (Pointer a1, Int32 a1len, Pointer a2, Pointer a3, Int32 n) {
        double *ds = (double*)a1;
        int *pi = (int*)a2;
        char *pc = (char*)a3;
        int i;
        double sum;

        sum = 0.0;
        for (i = 0; i < a1len; ++i) {
                sum += ds[i];
                ds[i] += n;
        }
        *pi = (int)sum;
        *pc = 'c';
        return 'c';
}
