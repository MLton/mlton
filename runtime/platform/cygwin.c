#include "platform.h"

#include "create.c"
#include "getrusage.c"
#include "mkdir2.c"
#include "mmap.c"
#include "setbintext.c"
#include "showMem.win32.c"
#include "totalRam.sysconf.c"
#include "virtualAlloc.c"

void decommit (void *base, size_t length) {
	if (MLton_Platform_CygwinUseMmap)
		smunmap (base, length);
	else
		decommitVirtual (base, length);
}

void *mmapAnon (void *start, size_t length) {
	if (MLton_Platform_CygwinUseMmap)
		return mmapAnonMmap (start, length);
	else
		return mmapAnonVirtual (start, length);
}

void release (void *base, size_t length) {
	if (MLton_Platform_CygwinUseMmap)
		smunmap (base, length);
	else
		releaseVirtual (base);
}
