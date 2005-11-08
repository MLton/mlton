#include "platform.h"

Int Posix_FileSys_readlink (Pointer p, Pointer b, Int n) {
        return readlink ((char*)p, (char*)b, n);
}
