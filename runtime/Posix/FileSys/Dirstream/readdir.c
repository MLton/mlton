#include <dirent.h>
#include <sys/types.h>
#include "mlton-posix.h"
#include "my-lib.h"

Cstring Posix_FileSys_Dirstream_readdir(Cpointer d) {
	struct dirent *e;
	
	e = readdir((DIR *) d);
	return (Cstring)((NULL == e) ? NULL : e->d_name);
}
