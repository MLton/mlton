#include "platform.h"

#include "platform/diskBack.unix.c"
#include "platform/displayMem.proc.c"
#include "platform/mmap-protect.c"
#include "platform/nonwin.c"
#include "platform/sysctl.c"
#include "platform/mmap.c"

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        return mmapAnonFlags (start, length, MAP_STACK);
}

void *GC_mmapAnonFlags (void *start, size_t length, int flags) {
	return mmapAnonFlags (start, length, flags | MAP_STACK);
}

void *GC_mmapAnonStack (void *start, size_t length, int prot,
                        size_t dead_low, size_t dead_high) {
	int flags = 0;
#ifdef MAP_STACK
	flags |= MAP_STACK;
#endif
	return GC_mmapAnonFlags_safe_protect (start, length, prot, flags,
	                                   dead_low, dead_high);
}
