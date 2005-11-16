#include "platform.h"

Cstring Posix_FileSys_getcwd (Pointer buf, Size n) {
        char *res = getcwd ((char*)buf, n);
        return (Cstring)res;
}
