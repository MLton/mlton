static void showMaps () {
	MEMORY_BASIC_INFORMATION buf;
	LPCVOID lpAddress;
	char *state = "<unset>";
	char *protect = "<unset>";

	for (lpAddress = 0; lpAddress < (LPCVOID)0x80000000; ) {
		VirtualQuery (lpAddress, &buf, sizeof (buf));

		switch (buf.Protect) {
		case PAGE_READONLY:
			protect = "PAGE_READONLY";
			break;
		case PAGE_READWRITE:
			protect = "PAGE_READWRITE";
			break;
		case PAGE_WRITECOPY:
			protect = "PAGE_WRITECOPY";
			break;
		case PAGE_EXECUTE:
			protect = "PAGE_EXECUTE";
			break;
		case PAGE_EXECUTE_READ:
			protect = "PAGE_EXECUTE_READ";
			break;
		case PAGE_EXECUTE_READWRITE:
			protect = "PAGE_EXECUTE_READWRITE";
			break;
		case PAGE_EXECUTE_WRITECOPY:
			protect = "PAGE_EXECUTE_WRITECOPY";
			break;
		case PAGE_GUARD:
			protect = "PAGE_GUARD";
			break;
		case PAGE_NOACCESS:
			protect = "PAGE_NOACCESS";
			break;
		case PAGE_NOCACHE:
			protect = "PAGE_NOCACHE";
			break;
		}
		switch (buf.State) {
		case MEM_COMMIT:
			state = "MEM_COMMIT";
			break;
		case MEM_FREE:
			state = "MEM_FREE";
			break;
		case MEM_RESERVE:
			state = "MEM_RESERVE";
			break;
		}
		fprintf(stderr, "0x%8x %10u  %s %s\n",
			(uint)buf.BaseAddress,
			(uint)buf.RegionSize,
			state, protect);
		lpAddress += buf.RegionSize;
	}
}

void showMem () {
	MEMORYSTATUS ms; 

	ms.dwLength = sizeof (MEMORYSTATUS); 
	GlobalMemoryStatus (&ms); 
	fprintf(stderr, "Total Phys. Mem: %ld\nAvail Phys. Mem: %ld\nTotal Page File: %ld\nAvail Page File: %ld\nTotal Virtual: %ld\nAvail Virtual: %ld\n",
			 ms.dwTotalPhys, 
			 ms.dwAvailPhys, 
			 ms.dwTotalPageFile, 
			 ms.dwAvailPageFile, 
			 ms.dwTotalVirtual, 
			 ms.dwAvailVirtual); 
	showMaps();
}
