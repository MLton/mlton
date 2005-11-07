#include "platform.h"

Cstring Posix_FileSys_getcwd (Pointer buf, Size n) {
        return (Cstring)(getcwd ((char*)buf, n));
}
