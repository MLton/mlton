#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

void MLton_exit (Int status) {
	GC_done (&gcState);
	exit (status);
}
