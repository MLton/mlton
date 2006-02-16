void *ssmmap (size_t length, size_t dead_low, size_t dead_high) {
        void *base,*low,*result,*high;

        base = smmap (length + dead_low + dead_high);
        low = base;
        if (mprotect (low, dead_low, PROT_NONE))
                diee ("mprotect failed");
        result = low + dead_low;
        if (mprotect (result, length, PROT_READ | PROT_WRITE | PROT_EXEC))
                diee ("mprotect failed");
        high = result + length;
        if (mprotect (high, dead_high, PROT_NONE))
                diee ("mprotect failed");
        return result;
}
