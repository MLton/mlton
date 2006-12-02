static inline void *mmapAnon (void *start, size_t length) {
        return mmap (start, length, PROT_READ | PROT_WRITE, 
                        MAP_PRIVATE | MAP_ANON, -1, 0);
}

static void munmap_safe (void *base, size_t length) {
        if (DEBUG_MEM)
                fprintf (stderr, "munmap_safe ("FMTPTR", %s)\n",
                                (uintptr_t)base,
                                uintmaxToCommaString (length));
        assert (base != NULL);
        if (0 == length)
                return;
        if (0 != munmap (base, length))
                diee ("munmap failed");
}
