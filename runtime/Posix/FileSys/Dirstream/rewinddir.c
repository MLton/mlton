#include <dirent.h>
#include <sys/types.h>
#include "mlton-posix.h"

void Posix_FileSys_Dirstream_rewinddir(Cpointer p) {
	rewinddir((DIR *) p);
}
