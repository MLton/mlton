#include "platform.h"

void MLton_allocTooLarge (void) {
        fprintf (stderr, "Out of memory: attempt to allocate more than %d bytes.\n", 0x7FFFFFFF);
        exit (2);
}
