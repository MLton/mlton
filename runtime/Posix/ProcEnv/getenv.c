#include "platform.h"

Cstring Posix_ProcEnv_getenv(NullString s) {
        return (Cstring)getenv((char *)s);
}
