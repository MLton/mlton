#include "platform.h"

Cstring Posix_ProcEnv_ttyname (Fd f) {
        return (Cstring)(ttyname (f));
}
