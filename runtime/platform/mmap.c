static inline void *mmapAnonFlags (void *start, size_t length, int flags) {
        return mmap (start, length, PROT_READ | PROT_WRITE, 
                        MAP_PRIVATE | MAP_ANON | flags, -1, 0);
}

static inline void *mmapAnon (void *start, size_t length) {
        return mmapAnonFlags (start, length, 0);
}

static void munmap_safe (void *base, size_t length) {
        assert (base != NULL);
        if (0 == length)
                return;
        if (0 != munmap (base, length))
                diee ("munmap failed");
}
