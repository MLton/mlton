#include "platform.h"

extern struct GC_state gcState;

Bool World_isOriginal() {
        return (Bool)(GC_getAmOriginal (&gcState));
}

void World_makeOriginal() {
        GC_setAmOriginal (&gcState, TRUE);
}
