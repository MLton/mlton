#include "platform.h"

Int Posix_FileSys_rename (Pointer p1, Pointer p2) {
        return rename ((char *) p1, (char *) p2);
}
