W32 totalRam (GC_state s) {
	W32 maxMem;
	W64 tmp;

	maxMem = 0x100000000llu - s->pageSize;
	tmp = sysconf (_SC_PHYS_PAGES) * (W64)s->pageSize;
	return (tmp >= maxMem) ? maxMem: (W32)tmp;
}
