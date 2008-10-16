#include "mmap.c"

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        return mmapAnon (start, length);
}
