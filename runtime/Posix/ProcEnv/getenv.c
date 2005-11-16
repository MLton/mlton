#include "platform.h"

Cstring Posix_ProcEnv_getenv(Pointer s) {
        char *res = getenv((char *)s);
        return (Cstring)res;
}
