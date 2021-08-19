// For `mremap` and `MREMAP_MAYMOVE`
#define _GNU_SOURCE

#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/displayMem.proc.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/use-mmap.c"

void *GC_mremap (void *start, size_t oldLength, size_t newLength) {
        return mremap (start, oldLength, newLength, MREMAP_MAYMOVE);
}

size_t GC_pageSize (void) {
        long int pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

/* sysconf(_SC_PHYS_PAGES) is not portable (mipsel uclibc) */
uintmax_t GC_physMem (void) {
        struct sysinfo si;
        if (sysinfo(&si) < 0)
                diee ("GC_physMem error: sysinfo failed");
        
        return (uintmax_t)si.totalram * (uintmax_t)si.mem_unit;
}
