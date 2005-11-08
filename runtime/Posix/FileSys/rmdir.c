#include "platform.h"

Int Posix_FileSys_rmdir (Pointer p) {
        return rmdir ((char *) p);
}
