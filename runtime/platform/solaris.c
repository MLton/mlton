#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysconf.c"
#include "platform/setenv.putenv.c"
#include "platform/use-mmap.c"

/* ------------------------------------------------- */
/*                        GC                         */
/* ------------------------------------------------- */

void GC_displayMem (void) {
        static char buffer[256];
        snprintf (buffer, cardof(buffer), "pmap %d\n", (int)(getpid ()));
        system (buffer);
}
