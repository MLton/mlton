W32 totalRam (GC_state s) {
	uint mem;
	int len, mib[2];
	
	mib[0] = CTL_HW;
	mib[1] = HW_PHYSMEM;
	len = sizeof(mem);
	if (-1 == sysctl (mib, 2, &mem, &len, NULL, 0))
		diee ("sysctl failed");
 	return mem;
}
