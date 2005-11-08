#include "platform.h"

Int Posix_FileSys_mkfifo (Pointer p, Word w) {
        return mkfifo ((char *) p, w);
}
