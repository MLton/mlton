#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

void GC_setHashConsDuringGC (Int b) {
        gcState.hashConsDuringGC = b;
}

void GC_setMessages (Int b) {
        gcState.messages = b;
}

void GC_setSummary (Int b) {
        gcState.summary = b;
}

void GC_setRusage () {
        gcState.rusageIsEnabled = TRUE;
}

void MLton_GC_pack () {
        GC_pack (&gcState);
}

void MLton_GC_unpack () {
        GC_unpack (&gcState);
}
