#include "platform.h"

Word32 Word8Vector_subWord32Rev (Pointer v, Int offset) {
        Word32 w;
        char *p;
        char *s;
        int i;

        p = (char*)&w;
        s = v + (offset * 4);
        for (i = 0; i < 4; ++i)
                p[i] = s[3 - i];
         return w;
}
