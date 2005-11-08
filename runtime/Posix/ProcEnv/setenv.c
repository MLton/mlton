#include "platform.h"

Int Posix_ProcEnv_setenv (Pointer s, Pointer v) {
        return setenv ((char*)s, (char*)v, 1);
}
