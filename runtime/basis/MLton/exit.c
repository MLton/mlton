#include "gc.h"
#include "mlton-basis.h"

extern struct GC_state gcState;

void MLton_exit(int status) {
	GC_done(&gcState);
	exit(status);
}
