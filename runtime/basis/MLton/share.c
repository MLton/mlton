#include "platform.h"

extern struct GC_state gcState;

void MLton_share (Pointer p) {
	GC_share (&gcState, p);
}

void MLton_shareAll () {
	GC_shareAll (&gcState);
}
