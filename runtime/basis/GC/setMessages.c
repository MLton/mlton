#include "gc.h"
#include "mlton-basis.h"

extern struct GC_state gcState;

void GC_setMessages(Int b) {
	gcState.messages = b;
}
