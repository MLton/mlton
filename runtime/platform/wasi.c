#include "platform.h"

#include "platform/mremap.c"
#include "platform/use-mmap.c"

/* WASI only implements a subset of POSIX, and given how it works it doesn't
 * make too much sense to try too hard to emulate missing functionality.
 *
 * Every function needed by the runtime library and not provided by WASI
 * is declared in wasi.h, but only a tiny number are implemented here.
 *
 * This means that any use of missing runtime functions from SML code will
 * result in linker errors at the end of compilation, rather than as a runtime
 * error later.
 *
 * The functions in this file are the bare minimum to get the GC working,
 * needed by every program compiled by MLton.
 */

size_t GC_pageSize (void) {
        long int pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

uintmax_t GC_physMem (void) {
        /* WASI doesn't provide a way to query actual physical memory, so pick
         * a reasonable amount (as of Feb 2024).
         */
        return 1 << 30;  /* 1 GiB */
}

void* GC_extendHead (void *base, size_t length) {
        return mmapAnon (base, length);
}

void* GC_extendTail (void *base, size_t length) {
        return mmapAnon (base, length);
}

void GC_displayMem (void) {
        printf ("GC_displayMem() not implemented on WASI\n");
}

void GC_diskBack_close (__attribute__ ((unused)) void *data) {
        die ("Disk-backed heap not supported on WASI");
}

void GC_diskBack_read (__attribute__ ((unused)) void *data,
                       __attribute__ ((unused)) pointer buf,
                       __attribute__ ((unused)) size_t size) {
        die ("Disk-backed heap not supported on WASI");
}

void *GC_diskBack_write (__attribute__ ((unused)) pointer buf,
                         __attribute__ ((unused)) size_t size) {
        die ("Disk-backed heap not supported on WASI");
}

int sigemptyset (sigset_t *set) {
        *set = (sigset_t) 0;
        return 0;
}
