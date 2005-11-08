#include "platform.h"

Int Posix_FileSys_chdir(Pointer p) {
        return chdir((char *) p);
}
