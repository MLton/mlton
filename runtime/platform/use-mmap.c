#include "mmap.c"

void GC_decommit (void *base, size_t length) {
        munmap_safe (base, length);
}

void GC_release (void *base, size_t length) {
        munmap_safe (base, length);
}

void *GC_mmapAnon (void *start, size_t length) {
        return mmapAnon (start, length);
}

void *GC_mmapAnon_safe (void *p, size_t length) {
        void *result;

        result = GC_mmapAnon (p, length);
        if ((void*)-1 == result) {
                GC_displayMem ();
                die ("Out of memory.");
        }
        return result;
}
