#include "platform.h"

extern struct GC_state gcState;

Int MLton_GC_gcTime() {
	return gcState.gcTime;
}
