#include "platform.h"

Int Posix_FileSys_access (Pointer f, Word w) {
        return access ((char *) f, w);
}
