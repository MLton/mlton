#include "platform.h"

Int Posix_FileSys_chmod (NullString p, Mode m) {
        return chmod ((char *) p, m);
}
