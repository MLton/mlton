#include <mach-o/dyld.h>
#include <mach-o/getsect.h>  // for get_etext()
#include <stdio.h>

#include "platform.h"

#include "getrusage.c"
#include "mkdir2.c"
#include "ssmmap.c"
#include "use-mmap.c"

void *getTextEnd () {
	return (void*)(get_etext ());
}

void *getTextStart () {
	unsigned long address;
	void *module;
	struct mach_header *mh;

	_dyld_lookup_and_bind ("_main", &address, &module);
	mh = _dyld_get_image_header_containing_address (address);
	return mh;
}

void showMem () {
	/* FIXME: this won't actually work. */
	static char buffer[256];

	sprintf (buffer, "/bin/cat /proc/%d/map\n", (int)getpid ());
	(void)system (buffer);
}

W32 totalRam (GC_state s) {
	int mem;
	size_t len;

	len = sizeof (int);
	if (-1 == sysctlbyname ("hw.physmem", &mem, &len, NULL, 0))
		diee ("sysctl failed");
	return mem;
}
