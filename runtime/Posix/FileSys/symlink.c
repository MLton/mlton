#include "platform.h"

Int Posix_FileSys_symlink (Pointer p1, Pointer p2) {
        return symlink ((char *) p1, (char *) p2);
}
