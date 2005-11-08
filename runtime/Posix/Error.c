#include "platform.h"

void Posix_Error_clearErrno () {
        errno = 0;
}

int Posix_Error_getErrno () {
        return errno;
}

Cstring Posix_Error_strerror (Int n) {
        return (Cstring)(strerror (n));
}
