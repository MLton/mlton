#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

void MLton_share (Pointer p) {
	GC_share (&gcState, p);
}
