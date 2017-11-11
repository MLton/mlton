void *GC_mmapAnon_safe_protect (void *start, size_t length, int prot,
                                size_t dead_low, size_t dead_high) {
        void *base,*low,*result,*high;

        base = GC_mmapAnon_safe (start, length + dead_low + dead_high);
        low = base;
        if (mprotect (low, dead_low, PROT_NONE))
                diee ("mprotect failed");
        result = (void*)((pointer)low + dead_low);
        if (mprotect (result, length, prot))
                diee ("mprotect failed");
        high = (void*)((pointer)result + length);
        if (mprotect (high, dead_high, PROT_NONE))
                diee ("mprotect failed");
        return result;
}
