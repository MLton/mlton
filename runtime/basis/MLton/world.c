#define _ISOC99_SOURCE

#include "platform.h"

extern struct GC_state gcState;

Bool World_isOriginal() {
	return gcState.isOriginal;
}


void World_makeOriginal() {
	gcState.isOriginal = TRUE;
}
