#include "platform.h"

void showMem () {
	static char buffer[256];

	sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
	(void)system (buffer);
}

#include "ssmmap.c"

W32 totalRam (GC_state s) {
	int mem, len;

	len = sizeof (int);
	if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
		diee ("sysctl failed");
	return mem;
}
