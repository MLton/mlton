static inline void *mmapAnonMmap (void *start, size_t length) {
	return mmap (start, length, PROT_READ | PROT_WRITE, 
			MAP_PRIVATE | MAP_ANON, -1, 0);
}

static void smunmap (void *base, size_t length) {
	if (DEBUG_MEM)
		fprintf (stderr, "smunmap (0x%08x, %s)\n",
				(uint)base,
				uintToCommaString (length));
	assert (base != NULL);
	if (0 == length)
		return;
	if (0 != munmap (base, length))
		diee ("munmap failed");
}
