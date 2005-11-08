#include "platform.h"

Int Posix_FileSys_chmod (Pointer p, Mode m) {
        return chmod ((char *) p, m);
}
