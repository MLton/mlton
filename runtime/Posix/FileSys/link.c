#include "platform.h"

Int Posix_FileSys_link (Pointer p1, Pointer p2) {
        return link ((char *) p1, (char *) p2);
}
