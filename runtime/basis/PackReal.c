#include "platform.h"

Real32 PackReal32_subVec (Pointer v, Int offset) {
        Real32 r;
        pointer p = (pointer)&r;
        pointer s = v + offset;
        int i;

        for (i = 0; i < 4; ++i)
                p[i] = s[i];
        return r;
}

Real32 PackReal32_subVecRev (Pointer v, Int offset) {
        Real32 r;
        pointer p = (pointer)&r;
        pointer s = v + offset;
        int i;

        for (i = 0; i < 4; ++i)
                p[i] = s[3 - i];
        return r;
}

Real64 PackReal64_subVec (Pointer v, Int offset) {
        Real64 r;
        pointer p = (pointer)&r;
        pointer s = v + offset;
        int i;

        for (i = 0; i < 8; ++i)
                p[i] = s[i];
        return r;
}

Real64 PackReal64_subVecRev (Pointer v, Int offset) {
        Real64 r;
        pointer p = (pointer)&r;
        pointer s = v + offset;
        int i;

        for (i = 0; i < 8; ++i)
                p[i] = s[7 - i];
        return r;
}

void PackReal32_update (Pointer a, Int offset, Real32 r) {
        pointer p = (pointer)&r;
        pointer s = a + offset;
        int i;

        for (i = 0; i < 4; ++i) {
                s[i] = p[i];
        }
}

void PackReal32_updateRev (Pointer a, Int offset, Real32 r) {
        pointer p = (pointer)&r;
        pointer s = a + offset;
        int i;

        for (i = 0; i < 4; ++i) {
                s[i] = p[3 - i];
        }
}

void PackReal64_update (Pointer a, Int offset, Real64 r) {
        pointer p = (pointer)&r;
        pointer s = a + offset;
        int i;

        for (i = 0; i < 8; ++i) {
                s[i] = p[i];
        }
}

void PackReal64_updateRev (Pointer a, Int offset, Real64 r) {
        pointer p = (pointer)&r;
        pointer s = a + offset;
        int i;

        for (i = 0; i < 8; ++i) {
                s[i] = p[7 - i];
        }
}
