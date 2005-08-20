#include "platform.h"

Int Posix_FileSys_rmdir (NullString p) {
        return rmdir ((char *) p);
}
