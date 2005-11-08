#include "platform.h"

Word Posix_FileSys_unlink (Pointer p) {
        return unlink ((char *) p);
}
