size_t GC_pageSize (void) {
        long int pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

uintmax_t GC_physMem (void) {
        size_t pageSize = GC_pageSize ();
        long int physPages;
        uintmax_t physMem;

        physPages = sysconf (_SC_PHYS_PAGES);
        if (physPages < 0)
                diee ("GC_physMem error: sysconf (_SC_PHYS_PAGES) failed");

        physMem = (uintmax_t)pageSize * (uintmax_t)physPages;
        return physMem;
}
