void *GC_mmapAnonFlags_safe_protect (void *start, size_t length,
                                     int prot, int flags,
                                     size_t dead_low,
                                     size_t dead_high) {
        void *base,*low,*result,*high;
        size_t totlen;

        totlen = length + dead_low + dead_high;
        base = GC_mmapAnonFlags_safe (start, totlen, flags);
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

void *GC_mmapAnon_safe_protect (void *start, size_t length, int prot,
                                size_t dead_low, size_t dead_high) {
	return GC_mmapAnonFlags_safe_protect (start, length,
	                                      prot, 0,
	                                      dead_low, dead_high);
}
