#include "platform/mmap.c"

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        return mmapAnon (start, length);
}

void *GC_mmapAnonFlags (void *start, size_t length, int flags) {
	return mmapAnonFlags(start, length, flags);
}

void *GC_mmapAnonStack (void *start, size_t length, int prot,
                        size_t dead_low, size_t dead_high) {
	return GC_mmapAnonFlags_safe_protect (start, length, prot, 0,
	                                      dead_low, dead_high);
}
