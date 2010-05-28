#define _GNU_SOURCE

#include "platform.h"

#include "diskBack.unix.c"
#include "displayMem.linux.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "use-mmap.c"
#include "sysconf.c"
#include "mremap.c"

void* GC_extendHead (void *base, size_t length) {
        return mmapAnon (base, length);
}

void* GC_extendTail (void *base, size_t length) {
        return mmapAnon (base, length);
}
