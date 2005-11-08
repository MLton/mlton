#include "platform.h"

Cstring Posix_ProcEnv_getenv(Pointer s) {
        return (Cstring)getenv((char *)s);
}
