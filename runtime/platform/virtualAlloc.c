static inline void releaseVirtual (void *base) {
	if (0 == VirtualFree (base, 0, MEM_RELEASE))
		die ("VirtualFree release failed");
}

static inline void decommitVirtual (void *base, size_t length) {
	if (0 == VirtualFree (base, length, MEM_DECOMMIT))
		die ("VirtualFree decommit failed");
}

static inline void *mmapAnonVirtual (void *start, size_t length) {
	void *res;

	res = VirtualAlloc ((LPVOID)start, length, MEM_COMMIT, PAGE_READWRITE);
	if (NULL == res)
		res = (void*)-1;
	return res;
}
