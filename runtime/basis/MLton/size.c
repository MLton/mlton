#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

Word MLton_size(Pointer p) {
	return GC_size(&gcState, p);
}
