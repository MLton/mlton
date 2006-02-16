#ifndef _C_MAIN_H_
#define _C_MAIN_H_

#include "main.h"
#include <qc--runtime.h>

#ifndef DEBUG_CMMCODEGEN
#define DEBUG_CMMCODEGEN FALSE
#endif

static inline Word32 returnAddressToFrameIndexCutTo (Word32 w) {
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "returnAddressToFrameIndex(0x%08x) starting\n", (uint)w);
        Cmm_Cont *k = (Cmm_Cont*)w;
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "\tCmm_YoungestActivation starting\n");
        Cmm_Activation a = Cmm_YoungestActivation (k);
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "\tCmm_YoungestActivation returning: ???\n");
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "\tCmm_GetDescriptor starting\n");
        Cmm_Dataptr ptr = Cmm_GetDescriptor (&a, 1);
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "\tCmm_GetDescriptor returning: 0x%08x\n", ptr);
        Word32 fi = *((Word32*)ptr);
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "returnAddressToFrameIndex(0x%08x) returning: 0x%08x\n", (uint)w, fi);
        return fi;
}

static inline Word32 returnAddressToFrameIndexReturn (Word32 w) {
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "returnAddressToFrameIndex(0x%08x) starting\n", (uint)w);
        Word32 fi = w;
        if (DEBUG_CMMCODEGEN)
                fprintf (stderr, "returnAddressToFrameIndex(0x%08x) returning: 0x%08x\n", (uint)w, fi);
        return fi;
}

#define Main(al, mg, mfs, mmc, pk, ps, mf, nt)                          \
static Word32 returnAddressToFrameIndex (Word32 w) {                    \
        if (nt) {                                                       \
                return returnAddressToFrameIndexCutTo(w);               \
        } else {                                                        \
                return returnAddressToFrameIndexReturn(w);              \
        }                                                               \
}                                                                       \
void mf ();                                                             \
void MLton_callFromC () {                                               \
        die ("cmm-main.h: MLton_callfromC\n");                          \
}                                                                       \
int main (int argc, char **argv) {                                      \
        Initialize (al, mg, mfs, mmc, pk, ps);                          \
        if (gcState.isOriginal) {                                       \
                real_Init();                                            \
                mf ();                                                  \
        } else {                                                        \
                die ("cmm-main.h: !gcState.isOriginal\n");              \
        }                                                               \
        return 1;                                                       \
}

#endif /* #ifndef _C_MAIN_H */
