static inline void releaseVirtual (void *base) {
        if (0 == VirtualFree (base, 0, MEM_RELEASE))
                die ("VirtualFree release failed");
}
