#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

void MLton_GC_setHashConsDuringGC (Int b) {
  GC_setHashConsDuringGC (&gcState, b);
}

void MLton_GC_setMessages (Int b) {
  GC_setMessages (&gcState, b);
}

void MLton_GC_setSummary (Int b) {
  GC_setSummary (&gcState, b);
}

void MLton_GC_setRusageMeasureGC (Int b) {
  GC_setRusageMeasureGC (&gcState, b);
}

void MLton_GC_pack () {
  GC_pack (&gcState);
}

void MLton_GC_unpack () {
  GC_unpack (&gcState);
}
