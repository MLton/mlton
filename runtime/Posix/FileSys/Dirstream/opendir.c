#include <dirent.h>
#include <sys/types.h>
#include "mlton-posix.h"

Cpointer Posix_FileSys_Dirstream_opendir(Cpointer p) {
	return (Cpointer)(opendir((char *) p));
}
