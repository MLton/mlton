#include "platform.h"

extern struct GC_state gcState;

Bool World_isOriginal(void) {
        return (Bool)(GC_getAmOriginal (&gcState));
}

void World_makeOriginal(void) {
        GC_setAmOriginal (&gcState, TRUE);
}
