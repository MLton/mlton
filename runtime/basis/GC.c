#include "platform.h"

extern struct GC_state gcState;

void GC_setMessages (Int b) {
	gcState.messages = b;
}

void GC_setSummary (Int b) {
	gcState.summary = b;
}
