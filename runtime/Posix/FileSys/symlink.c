#include "platform.h"

Int Posix_FileSys_symlink (NullString p1, NullString p2) {
        return symlink ((char *) p1, (char *) p2);
}
