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

void MLton_GC_pack () {
	GC_pack (&gcState);
}

void MLton_GC_unpack () {
	GC_unpack (&gcState);
}
