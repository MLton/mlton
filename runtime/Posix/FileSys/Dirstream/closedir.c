#include <dirent.h>
#include <sys/types.h>
#include "mlton-posix.h"

Int Posix_FileSys_Dirstream_closedir(Cpointer p) {
	return (Int)(closedir((DIR *) p));
}
