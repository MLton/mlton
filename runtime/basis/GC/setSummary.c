#include "gc.h"
#include "mlton-basis.h"

extern struct GC_state gcState;

void GC_setSummary(Int b) {
	gcState.summary = b;
}
