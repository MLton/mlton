#include "platform.h"

void Stdio_print (Pointer s) {
        uintmax_t size = GC_getArrayLength (s);
        if (0 == size)
                return;
        while (1 != fwrite (s, (size_t)size, 1, stderr))
                /* nothing */;
}
