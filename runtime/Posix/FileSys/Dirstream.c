#include "platform.h"

Int Posix_FileSys_Dirstream_closedir (Cpointer p) {
	return (Int)(closedir ((DIR *) p));
}

Cpointer Posix_FileSys_Dirstream_opendir (Cpointer p) {
	return (Cpointer)(opendir ((char *) p));
}

Cstring Posix_FileSys_Dirstream_readdir (Cpointer d) {
	struct dirent *e;
	
	e = readdir ((DIR *) d);
	return (Cstring)((NULL == e) ? NULL : e->d_name);
}

void Posix_FileSys_Dirstream_rewinddir (Cpointer p) {
	rewinddir ((DIR *) p);
}
