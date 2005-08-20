#include "platform.h"

Int Posix_FileSys_chdir(Cpointer p) {
        return chdir((char *) p);
}
