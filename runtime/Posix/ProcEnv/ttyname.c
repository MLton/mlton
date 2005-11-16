#include "platform.h"

Cstring Posix_ProcEnv_ttyname (Fd f) {
        char *res = ttyname (f);
        return (Cstring)res;
}
