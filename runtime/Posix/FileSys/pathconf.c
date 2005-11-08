#include "platform.h"

Int Posix_FileSys_pathconf (Pointer p, Int n) {
        return pathconf ((char *)p, n);
}
