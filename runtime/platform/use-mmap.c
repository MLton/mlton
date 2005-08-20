#include "mmap.c"

void decommit (void *base, size_t length) {
        smunmap (base, length);
}

void release (void *base, size_t length) {
        smunmap (base, length);
}

void *mmapAnon (void *start, size_t length) {
        return mmapAnonMmap (start, length);
}
